package shipreq.webapp.client.project.app.pages.config.tags

import shipreq.webapp.base.jsfacade.{ReactColor, TinyColor}

object TagPalette {

  // This was designed in /Design/prototype-ui/tag-colours.haml

  private def hsl(h: Int, s: Double, l: Double): String =
    TinyColor(TinyColor.Hsl(h, s, l)).toHex()

  private val colours: Seq[String] =
    Seq(
      hsl(  1,  .95, .86), // [l1 ] light red
      hsl( 26,  .95, .80), // [l2 ] light strong orange
      hsl( 47,  .88, .70), // [l3 ] light weak orange
      hsl( 59,  .84, .74), // [l4 ] light yellow
      hsl( 70,  .60, .76), // [l5 ] light weak green
      hsl(121,  .40, .80), // [l6 ] light strong green
      hsl(180,  .55, .80), // [l7 ] light aqua
      hsl(211,  .92, .88), // [l8 ] light blue
      hsl(257,  .87, .90), // [l9 ] light purple
      hsl(310,  .95, .91), // [l10] light pink

      hsl(  2, 1   , .48), // [c1 ] main red
      hsl( 26, 1   , .50), // [c2 ] main strong orange
      hsl( 47, 1   , .51), // [c3 ] main weak orange
      hsl( 59, 1   , .48), // [c4 ] main yellow
      hsl( 69, 1   , .47), // [c5 ] main weak green
      hsl(121, 1   , .33), // [c6 ] main strong green
      hsl(180, 1   , .43), // [c7 ] main aqua
      hsl(211, 1   , .48), // [c8 ] main blue
      hsl(261, 1   , .48), // [c9 ] main purple
      hsl(310, 1   , .49), // [c10] main pink

      hsl(  1,  .95, .36), // [d1 ] dark red
      hsl( 26,  .95, .41), // [d2 ] dark strong orange
      hsl( 47,  .95, .43), // [d3 ] dark weak orange
      hsl( 59,  .95, .43), // [d4 ] dark yellow
      hsl( 69,  .95, .39), // [d5 ] dark weak green
      hsl(121,  .95, .22), // [d6 ] dark strong green
      hsl(180,  .95, .32), // [d7 ] dark aqua
      hsl(211,  .95, .40), // [d8 ] dark blue
      hsl(261,  .95, .38), // [d9 ] dark purple
      hsl(310,  .95, .38), // [d10] dark pink
    )

  val forGithubPicker: ReactColor.Github.Colours =
    ReactColor.Github.Colours(colours, "262px")
}
