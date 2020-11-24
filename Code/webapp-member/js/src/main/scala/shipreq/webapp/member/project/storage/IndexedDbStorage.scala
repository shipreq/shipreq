package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import shipreq.base.util.MutableRef
import shipreq.webapp.base.protocol.Version
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.project.library.{Cache, CacheJs, ProjectLibrary}
import shipreq.webapp.member.protocol.binary.{BinaryFormat, Compression, Encryption}
import shipreq.webapp.member.protocol.indexeddb.IndexedDb.{Txn, TxnDsl, VersionChange}
import shipreq.webapp.member.protocol.indexeddb._

/** Uses IndexedDB as the client-side storage mechanism.
 *
 * Format:
 *
 * {{{
 *   Project store:
 *     - ord : <encrypted (latest) project>
 *     - all milestones plus the latest value are stored
 * }}}
 */
final class IndexedDbStorage(db            : IndexedDb.Database,
                             isAvailableVar: MutableRef[Boolean],
                             schema        : IndexedDbStorage.Schema,
                             plCache       : Cache) extends ClientSideStorage.ReadWrite {

  import IndexedDbStorage.Internals._

  override val isAvailable: CallbackTo[Boolean] =
    CallbackTo(isAvailableVar.value)

  override val getProjectLibraryOrd: AsyncCallback[Option[EventOrd.Latest]] =
    for {
      ords <- db.getAllKeys(schema.project.store)
    } yield ords.maxOption.map(_.asLatest)

  override val getProjectLibrary: AsyncCallback[Option[ProjectLibrary]] =
    for {
      projects <- db.getAllValues(schema.project.store)
    } yield ProjectLibrary.load(projects, plCache)

  override def saveProjectLibrary(pl: ProjectLibrary): AsyncCallback[Unit] = {
    import schema.project.store

    def update(add: Iterable[(EventOrd, store.Value)], delete: Set[EventOrd])(txn: TxnDsl): Txn[Unit] =
      for {
        s <- txn.objectStore(store)
        _ <- txn.traverse_(delete)(s.delete)
        _ <- txn.traverse_(add)(x => s.put(x._1, x._2))
      } yield ()

    for {
      alreadyStored <- db.getAllKeys(store)
      (add, delete) <- AsyncCallback.pure(savePlan(pl, alreadyStored))
      addBlobs      <- AsyncCallback.traverse(add.toList) { case (o, p) => store.encode(p).map((o, _)) }
      _             <- db.transactionRW(store)(update(addBlobs, delete))
    } yield ()
  }
}

object IndexedDbStorage {

  def apply(idb         : IndexedDb,
            ctx         : ClientSideStorage.Context,
            encryption  : Encryption): AsyncCallback[IndexedDbStorage] =
    nonStandard(idb, ctx, encryption)

  private[storage] def nonStandard(idb         : IndexedDb,
                                   ctx         : ClientSideStorage.Context,
                                   encryption  : Encryption,
                                   dbNamePrefix: String = "",
                                   plCache     : Cache = CacheJs(),
                                  ): AsyncCallback[IndexedDbStorage] = {

    val schema = new Schema(ctx, encryption, dbNamePrefix)

    val isAvailable = MutableRef.boolean(true)

    val openCallbacks = IndexedDb.OpenCallbacks(
      upgradeNeeded = schema.upgradeNeeded,
      versionChange = _ => Callback.empty,
      closed        = Callback { isAvailable.value = false },
    )

    idb.open(schema.dbName, schema.dbVer)(openCallbacks)
      .map(new IndexedDbStorage(_, isAvailable, schema, plCache))
  }

  // ===================================================================================================================

  /** Save projects as milestones every n events.
   *
   * See https://shipreq.com/project/d6My#/reqs/DE-3
   */
  final val MilestonesEvery = 4000

  @elidable(elidable.FINEST)
  private[storage] def assertMilestones(inMem: Int, onDisk: Int): Unit =
    if ((inMem % onDisk) != 0)
      throw new ExceptionInInitializerError(
        s"""
           |
           |MILESTONES ARE NOT ALIGNED.
           |
           |We're retaining every $inMem in memory but every $onDisk on disk.
           |In-memory milestones should be a multiple of on-disk milestones, else it's likely to be grossly inefficient.
           |
           |""".stripMargin
      )

  assertMilestones(
    inMem = CacheJs.MilestonesEvery,
    onDisk = MilestonesEvery)

  // ===================================================================================================================

  private[IndexedDbStorage] final class Schema(ctx: ClientSideStorage.Context, encryption: Encryption, dbNamePrefix: String) {
    import SafePickler.ConstructionHelperImplicits._

    val dbName = IndexedDb.DatabaseName(dbNamePrefix + ctx.namespace)
    val dbVer  = 1

    def upgradeNeeded(c: VersionChange): Callback =
      c.createObjectStore(1, project.store)

    object project {

      val ver = Version.fromInts(1, 0) // Bump this when any of following imports change
      import shipreq.webapp.member.project.protocol.binary.v2.Rev0.picklerProject

      implicit val pickler: SafePickler[Project] =
        picklerProject
          .asVersion(ver)
          .withMagicNumbers(0x8CF0655B, 0x5A8218EB)

      val format: BinaryFormat[Project] =
        BinaryFormat.versioned(
          BinaryFormat.pickleCompressEncrypt(Compression.maxNoHeaders, encryption))

      val valueCodec: ValueCodec.Async[Project] =
        ValueCodec.Async.binary.xmapBinaryFormat(format)

      val keyCodec: KeyCodec[EventOrd] =
        KeyCodec.int.xmap(EventOrd.apply)(_.value)

      val store: ObjectStoreDef.Async[EventOrd, Project] =
        ObjectStoreDef.Async("project", keyCodec, valueCodec)
    }

  } // Protocols

  // ===================================================================================================================

  private[storage] final val Internals = new Internals(MilestonesEvery)

  private[storage] final class Internals(milestonesEvery: Int) {

    def isMilestone(o: EventOrd): Boolean =
      (o.value % milestonesEvery) == 0

    def isMilestone(p: Project): Boolean =
      (p.ordAsInt % milestonesEvery) == 0

    def savePlan(pl: ProjectLibrary, alreadyStored: ArraySeq[EventOrd]) = {

      // Store all eligible milestones
      var wantStored: Map[EventOrd, Project] =
        pl.cache
          .iterator()
          .filter(isMilestone)
          .flatten(p => p.ord.map(o => o.asEventOrd -> p))
          .toMap

      // Store the latest project
      for (o <- pl.ord)
        wantStored = wantStored.updated(o.asEventOrd, pl.latest)

      // Inspect all existing content
      var retain              = Set.empty[Int]
      var delete              = Set.empty[EventOrd]
      var latestAlreadyStored = new EventOrd(-1)

      for (ord <- alreadyStored) {
        if (ord > latestAlreadyStored)
          latestAlreadyStored = ord

        if (isMilestone(ord))
          retain += ord.value
        else if (!wantStored.contains(ord))
          delete += ord
      }

      // Ensure we don't regress if something more recent is already stored
      if (latestAlreadyStored.value > 0 && latestAlreadyStored > pl.latest) {
        retain += latestAlreadyStored.value
        delete -= latestAlreadyStored
        for (o <- pl.ord)
          if (!isMilestone(o))
            wantStored = wantStored - o
      }

      // Add everything we want store that isn't stored already
      val add: Map[EventOrd, Project] =
        wantStored.filter(x => !retain.contains(x._1.value))

      (add, delete)
    }

  }

}