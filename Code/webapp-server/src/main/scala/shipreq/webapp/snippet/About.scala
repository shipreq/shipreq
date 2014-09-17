package shipreq.webapp.snippet

import xml.{Unparsed, NodeSeq, Text}
import net.liftweb.util.Helpers._
import net.liftweb.util.CssSel
import shipreq.webapp.lib.ScalazSubset._

class About {

  def attribution: CssSel = {
    sealed case class Licence(name: String, url: String) {
      val xml: NodeSeq = <a class="licence" href={url}>{name}</a>
    }

    sealed case class Attribution(name: String, url: String, copyright: Option[NodeSeq], licences: Set[Licence]) {
      def nameXml =
        <a href={url}>{name}</a>

      def copyrightSync(c: NodeSeq): NodeSeq =
        Unparsed(c.toString.replaceAll("""(?i)(?:(?:(?:copyright )?(?:©|\(c\)))|copyright)(?= \d)""", "©"))

      def copyrightXml: Option[NodeSeq] =
        copyright.map(c => <span class="copyright">{copyrightSync(c)}</span>)

      def licenceXml: Option[NodeSeq] =
        if (licences.isEmpty) None else
          Some(<span class="licence">Licenced under {licences.toList.sortBy(_.name).map(_.xml).intercalate(Text(", "))}.</span>)

      def render: NodeSeq =
        (nameXml, copyrightXml, licenceXml) match {
          case (n, None, None)       => <xml:group>{n}.</xml:group>
          case (n, None, Some(l))    => <xml:group>{n} – {l}</xml:group>
          case (n, Some(c), Some(l)) => <xml:group>{n} – {c} – {l}</xml:group>
          case (n, Some(c), None)    => <xml:group>{n} – {c}</xml:group>
        }
    }

    val Apache2 = Licence("Apache License 2.0", "http://opensource.org/licenses/Apache-2.0")
    val BSD_2 = Licence("BSD 2-Clause Licence", "http://opensource.org/licenses/BSD-2-Clause")
    val BSD_3 = Licence("BSD 3-Clause Licence", "http://opensource.org/licenses/BSD-3-Clause")
    val EPLv1 = Licence("Eclipse Public License v1.0", "http://opensource.org/licenses/EPL-1.0")
    val GPLv2 = Licence("GPL v2", "http://opensource.org/licenses/GPL-2.0")
    val LGPLv21 = Licence("LGPL v2.1", "http://opensource.org/licenses/LGPL-2.1")
    val MIT = Licence("MIT Licence", "http://opensource.org/licenses/MIT")

    val alist = List(
      Attribution("Scala.js",
        "http://www.scala-js.org/",
        Some(<xml:group>Copyright (c) 2013-2014 EPFL</xml:group>),
        Set(BSD_3))
      ,
      Attribution("React",
        "http://facebook.github.io/react/",
        Some(Text("Copyright 2013-2014 Facebook, Inc.")),
        Set(Apache2))
      ,
      Attribution("scalajs-react",
        "https://github.com/japgolly/scalajs-react",
        Some(Text("Copyright © 2014 David Barri")),
        Set(Apache2))
      ,
      Attribution("Scalatags",
        "https://github.com/lihaoyi/scalatags",
        Some(Text("Copyright (c) 2013, Li Haoyi")),
        Set(MIT))
      ,
      Attribution("jQuery",
        "http://jquery.com/",
        Some(Text("Copyright 2013 jQuery Foundation and other contributors.")),
        Set(MIT))
      ,
      Attribution("jQuery.autosize",
        "http://www.jacklmoore.com/autosize/",
        None,
        Set(MIT))
      ,
      Attribution("jQuery.timeago",
        "http://timeago.yarp.com/",
        Some(<xml:group>Copyright © 2008-2013 <a href="http://ryan.mcgeary.org">Ryan McGeary</a> (<a href="http://twitter.com/rmm5t">@rmm5t</a>).</xml:group>),
        Set(MIT))
      ,
      Attribution("jQuery.liveQuery",
        "https://github.com/brandonaaron/livequery",
        Some(<xml:group>Copyright (c) 2010 <a href="http://brandonaaron.net">Brandon Aaron</a>.</xml:group>),
        Set(MIT, GPLv2))
      ,
      Attribution("ZeroClipboard",
        "http://zeroclipboard.org/",
        Some(Text("Copyright (c) 2013 Jon Rohan, James M. Greene.")),
        Set(MIT))
      ,
      Attribution("Mousetrap",
        "http://craig.is/killing/mice",
        None,
        Set.empty)
      ,
      Attribution("GraphViz",
        "http://www.graphviz.org/",
        None,
        Set(EPLv1))
      ,
      Attribution("Viz.js",
        "https://github.com/mdaines/viz.js/",
        Some(Text("Copyright (c) 2012 Michael Daines.")),
        Set(MIT))
      ,
      Attribution("Twitter Bootstrap",
        "http://getbootstrap.com/",
        Some(Text("Copyright 2013 Twitter, Inc.")),
        Set(Apache2))
      ,
      Attribution("Scala",
        "http://www.scala-lang.org/",
        Some(<xml:group>Copyright (c) 2002-2014 EPFL, Copyright (c) 2011-2014 Typesafe, Inc.</xml:group>),
        Set(BSD_3))
      ,
      Attribution("Lift",
        "http://liftweb.net/",
        Some(Text("Copyright © 2010-2014 WorldWide Conferencing, LLC.")),
        Set(Apache2))
      ,
      Attribution("Scalaz",
        "https://github.com/scalaz/scalaz",
        None,
        Set.empty)
      ,
      Attribution("Logback",
        "http://logback.qos.ch/",
        Some(Text("Copyright (C) 1999-2012, QOS.ch. All rights reserved.")),
        Set(EPLv1, LGPLv21))
      ,
      Attribution("Slick",
        "http://slick.typesafe.com/",
        Some(<xml:group>Copyright © 2011-2012 <a href="http://typesafe.com">Typesafe, Inc.</a></xml:group>),
        Set(BSD_2))
      ,
      Attribution("FlyWay",
        "http://flywaydb.org/",
        Some(<xml:group>© 2010-2014 <a href="http://axelfontaine.com">Axel Fontaine</a> and the <a href="http://flywaydb.org/contribute/hallOfFame.html">many contributors</a>.</xml:group>),
        Set(Apache2))
      ,
      Attribution("Shiro",
        "http://shiro.apache.org/",
        Some(Text("Copyright © 2008-2013 The Apache Software Foundation.")),
        Set(Apache2))
      ,
      Attribution("SLF4J",
        "http://www.slf4j.org/",
        Some(Text("Copyright (c) 2004-2013 QOS.ch.")),
        Set(MIT))
      ,
      Attribution("BoneCP",
        "http://jolbox.com/",
        Some(Text("Copyright 2010 Wallace Wadge.")),
        Set(Apache2))
      ,
      Attribution("Guava",
        "https://code.google.com/p/guava-libraries/",
        None,
        Set(Apache2))
      ,
      Attribution("Scalate",
        "http://scalate.fusesource.org/",
        None,
        Set(Apache2))
      ,
      Attribution("Apache Commons",
        "http://commons.apache.org/proper/commons-lang/",
        Some(Text("Copyright 2001-2013 The Apache Software Foundation.")),
        Set(Apache2))
      ,
      Attribution("PostgreSQL JDBC driver",
        "http://jdbc.postgresql.org/",
        Some(Text("Copyright (c) 1997-2011, PostgreSQL Global Development Group.")),
        Set(BSD_3))
      ,
      Attribution("DejaVu fonts",
        "http://dejavu-fonts.org/",
        Some(<xml:group>Copyright © 2003 by Bitstream, Inc. All Rights Reserved. Bitstream Vera is a trademark of Bitstream, Inc. Glyphs imported from <a href="http://dejavu-fonts.org/wiki/Bitstream_Vera_derivatives#Arev_Fonts">Arev fonts</a> are copyright © 2006 by Tavmjong Bah. All Rights Reserved.</xml:group>),
        Set.empty)
    )

    val renderedRows = alist.sortBy(_.name.toLowerCase).map(_.render)
    "* *" #> renderedRows
  }
}
