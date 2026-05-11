package shipreq.webapp.member.project.storage

object IndexedDbStorageLaws extends ClientSideStorageLaws {

  override protected def createInstance =
    IndexedDbStorageTest.newStorage(_, _)

}
