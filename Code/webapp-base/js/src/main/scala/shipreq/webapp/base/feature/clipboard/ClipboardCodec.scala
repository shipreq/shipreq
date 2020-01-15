package shipreq.webapp.base.feature.clipboard

/** A protocol to read from, and write to, the clipboard for a specific data type. */
final class ClipboardCodec[A](val write: A => ClipboardData,
                              val read: ClipboardData => Option[A]) {

  def xmap[B](r: A => B)(w: B => A): ClipboardCodec[B] =
    ClipboardCodec(read.andThen(_.map(r)))(write compose w)

  def correct(f: A => A): ClipboardCodec[A] =
    xmap(f)(f)

  def readOrUse(cd: Option[ClipboardData], use: => A): A =
    cd.flatMap(read).getOrElse(use)
}

object ClipboardCodec {

  def apply[A](read: ClipboardData => Option[A])
              (write: A => ClipboardData): ClipboardCodec[A] =
    new ClipboardCodec(write, read)

  def total[A](read: ClipboardData => A)
              (write: A => ClipboardData): ClipboardCodec[A] =
    apply(read.andThen(Some(_)))(write)

  lazy val string: ClipboardCodec[String] =
    total(_.text)(ClipboardData.apply)
}