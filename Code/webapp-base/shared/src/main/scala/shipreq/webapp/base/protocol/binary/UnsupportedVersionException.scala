package shipreq.webapp.base.protocol.binary

import shipreq.webapp.base.protocol.Version

final case class UnsupportedVersionException(found: Version, maxSupported: Version)
  extends RuntimeException(s"${found.verStr} not supported. ${maxSupported.verStr} is the max supported.")