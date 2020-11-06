package shipreq.webapp.member.test.project

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.Atom.DisplayReqRef
import shipreq.webapp.member.project.text.{Text => T, _}
import shipreq.webapp.member.test.project.ProjectDsl._
import shipreq.webapp.member.test.project.SampleProject.{project => project0}
import shipreq.webapp.member.test.project.UnsafeTypes._

/**
 * Builds on SampleProject #1 (not #2) to add:
 *   - generic reqs (dead & live)
 *   - req code groups
 *   - dead req codes
 *   - a bit of rich text.
 *   - deletion reasons.
 */
object SampleProject3 {

  trait Values extends SampleProject.Values {
    val frs = (0 to 10).iterator.map(i => GenericReqId(i + 1000)).toVector
    val mfs = (0 to 28).iterator.map(i => GenericReqId(i + 1100)).toVector
    val cos = (0 to 10).iterator.map(i => GenericReqId(i + 1200)).toVector
  }
  object Values extends Values
  import Values._

  val inlineIssueDesc = {
    import T.InlineIssueDesc._
    apply(Literal("Pending "), ReqRef(mfs(26), DisplayReqRef.AsId))
  }

  lazy val project: Project = {

    def fr1Title = {
      import T.GenericReqTitle._
      apply(
        EmailAddress("japgolly@gmail.com"), Literal(" is on "), WebAddress("https://github.com"),
        Literal(" cos of "), ReqRef(mfs(6), DisplayReqRef.AsId), Literal(" "), Issue(1, T.empty),
        TeX("c = \\pm\\sqrt{a^2 + b^2}")
      )
    }
    def fr2Title = {
      import T.GenericReqTitle._
      apply(Issue(2, inlineIssueDesc), Literal(". "), ReqRef(mfs(28), DisplayReqRef.AsId), Literal(" is dead."))
    }

    val contentByDsl = (
      GReq(reqType = mf, id = mfs( 1), title = "Use Case Editor"                      , codes = Set("uce")).tag(priHigh).tag(v10)
    + GReq(reqType = mf, id = mfs( 2), title = "Anonymous Share"                      ).tag(priMed).tag(v10)
    + GReq(reqType = mf, id = mfs( 3), title = "Export (PDF, XLS)"                    ).tag(priHigh)
    + GReq(reqType = mf, id = mfs( 4), title = "Templates"                            ).tag(priMed)
    + GReq(reqType = mf, id = mfs( 5), title = "Field Customisation"                  ).tag(priHigh).tag(wip)
    + GReq(reqType = mf, id = mfs( 6), title = "Incompletions"                        ).tag(priMed).tag(wip)
    + GReq(reqType = mf, id = mfs( 7), title = "Organisation"                         ).tag(priHigh).tag(wip).tag(v1x).tag(v10)
    + GReq(reqType = mf, id = mfs( 8), title = "History/Audit"                        ).tag(priMed)
    + GReq(reqType = mf, id = mfs( 9), title = "Collaboration: authoring"             ).tag(priHigh)
    + GReq(reqType = mf, id = mfs(10), title = "Collaboration: stakeholders"          ).tag(priHigh)
    + GReq(reqType = mf, id = mfs(11), title = "Collaboration: change mgnt & approval").tag(priHigh)
    + GReq(reqType = mf, id = mfs(12), title = "Low-level Requirements"               ).tag(wip).tag(priHigh)
    + GReq(reqType = mf, id = mfs(13), title = "Requirement Relationships"            ).tag(wip).tag(priHigh)
    + GReq(reqType = mf, id = mfs(14), title = "Text-generated Diagrams"              ).tag(priMed)
    + GReq(reqType = mf, id = mfs(15), title = "Matrixes"                             ).tag(priMed)
    + GReq(reqType = mf, id = mfs(16), title = "CRUDL Matrix"                         ).tag(priLow)
    + GReq(reqType = mf, id = mfs(17), title = "Undo & Auto-save"                     ).tag(priMed)
    + GReq(reqType = mf, id = mfs(18), title = "Data dictionary"                      ).tag(priLow)
    + GReq(reqType = mf, id = mfs(19), title = "Glossary", live = Dead                ).tag(priLow)
    + GReq(reqType = mf, id = mfs(20), title = "Generic artifact storage"             ).tag(priMed)
    + GReq(reqType = mf, id = mfs(21), title = "Doc authoring (V&S, URD, SRS)"        ).tag(priMed)
    + GReq(reqType = mf, id = mfs(22), title = "High-level Requirements"              ).tag(priMed).tag(wip)
    + GReq(reqType = mf, id = mfs(23), title = "Import external requirements"         ).tag(priMed)
    + GReq(reqType = mf, id = mfs(24), title = "Requirement Lint"                     ).tag(priMed).tag(v3x)
    + GReq(reqType = mf, id = mfs(25), title = "Search"                               ).tag(priMed)
    + GReq(reqType = mf, id = mfs(26), title = "Mass text modification (replace)"     ).tag(priLow)
    + GReq(reqType = mf, id = mfs(27), title = "External references"                  ).tag(priLow).impSrc(frs(2))
    + GReq(reqType = mf, id = mfs(28), title = "Entities", live = Dead                ).tag(priMed)

    + GReq(reqType = fr, id = frs(1), title = fr1Title, codes = Set("uce.sample.1", "uce.sample.1b", "demo.whatever")).impSrc(mfs(12), mfs(19))
    + GReq(reqType = fr, id = frs(2), title = fr2Title, codes = Set("uce.sample.2")).impSrc(mfs(1), mfs(13), mfs(22), frs(1))
    + RCGroup("demo", title = T.CodeGroupTitle(T.CodeGroupTitle.Literal("Demo group header")))

    + GReq(reqType = co, id = cos(1), live = Dead, title = "Search entities!").impSrc(mfs(28), mfs(25)).tag(v10, v3x)
    + GReq(reqType = co, id = cos(2), live = Dead, title = "Entity-search should consider low-level reqs").impSrc(cos(1), frs(1))

    + DeadReqCode("dead.ref", oldReqId = mfs(7))
    + DeadReqCode("dead.group")
    )

    val dr = DeletionReasons(
      Vector("Who needs a use case edtior?!", "Bobsaidso.", "Bob said so."),
      DeletionReasons.emptyReqApplication
        .add(mfs(1), 0)
        .add(cos(2), None)
        .add(cos(2), 1)
        .add(cos(2), 2)
    )

    Project.deletionReasons.set(dr)(contentByDsl ! project0)
  }

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)

  // Real web workers aren't supported in tests. PhantomJS doesn't support them and until we move off PhantomJS, here
  // are some pre-generated graphs to feed the TestWebWorkerClient. To generate more or re-generate:
  //
  // 1) Generate the DOT via ProjectImpGraphTest.extractDot
  // 2) Save as a GV and use GraphViz to generate an SVG (dot -Tsvg -oblah.svg blah.gv)
  object reqGraph {

    val showDead =
      """
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Generated by graphviz version 2.44.1 (0)
 -->
<!-- Title: G Pages: 1 -->
<svg width="2294pt" height="260pt"
 viewBox="0.00 0.00 2294.04 260.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 256)">
<title>G</title>
<!-- 1202 -->
<g id="CO&#45;2" class="node">
<title>1202</title>
<ellipse fill="#dddddd" stroke="#777777" cx="190.95" cy="-90" rx="34.39" ry="18"/>
<text text-anchor="middle" x="190.95" y="-86.3" font-family="Times,serif" font-size="14.00" fill="#666666">CO&#45;2</text>
</g>
<!-- 1201 -->
<g id="CO&#45;1" class="node">
<title>1201</title>
<ellipse fill="#dddddd" stroke="#777777" cx="140.95" cy="-162" rx="34.39" ry="18"/>
<text text-anchor="middle" x="140.95" y="-158.3" font-family="Times,serif" font-size="14.00" fill="#666666">CO&#45;1</text>
</g>
<!-- 1201&#45;&gt;1202 -->
<g id="CO&#45;1&#45;&#45;CO&#45;2" class="edge">
<title>1201&#45;&gt;1202</title>
<path fill="none" stroke="#bbbbbb" stroke-dasharray="5,2" d="M152.54,-144.76C158.79,-136.02 166.62,-125.05 173.58,-115.31"/>
<polygon fill="#bbbbbb" stroke="#bbbbbb" points="176.45,-117.32 179.41,-107.15 170.75,-113.25 176.45,-117.32"/>
</g>
<!-- 1001 -->
<g id="FR&#45;1" class="node">
<title>1001</title>
<ellipse fill="#d5a8c9" stroke="#222222" cx="240.95" cy="-162" rx="32.49" ry="18"/>
<text text-anchor="middle" x="240.95" y="-158.3" font-family="Times,serif" font-size="14.00">FR&#45;1</text>
</g>
<!-- 1001&#45;&gt;1202 -->
<g id="FR&#45;1&#45;&#45;CO&#45;2" class="edge">
<title>1001&#45;&gt;1202</title>
<path fill="none" stroke="#bbbbbb" stroke-dasharray="5,2" d="M229.6,-145.12C223.36,-136.38 215.48,-125.35 208.47,-115.54"/>
<polygon fill="#bbbbbb" stroke="#bbbbbb" points="211.25,-113.41 202.59,-107.31 205.56,-117.48 211.25,-113.41"/>
</g>
<!-- 1002 -->
<g id="FR&#45;2" class="node">
<title>1002</title>
<ellipse fill="#d5a8c9" stroke="#222222" cx="373.95" cy="-90" rx="32.49" ry="18"/>
<text text-anchor="middle" x="373.95" y="-86.3" font-family="Times,serif" font-size="14.00">FR&#45;2</text>
</g>
<!-- 1001&#45;&gt;1002 -->
<g id="FR&#45;1&#45;&#45;FR&#45;2" class="edge">
<title>1001&#45;&gt;1002</title>
<path fill="none" stroke="#222222" d="M263.81,-148.97C285.32,-137.65 317.72,-120.59 341.82,-107.91"/>
<polygon fill="#222222" stroke="#222222" points="343.72,-110.87 350.94,-103.11 340.46,-104.67 343.72,-110.87"/>
</g>
<!-- 1127 -->
<g id="MF&#45;27" class="node">
<title>1127</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="373.95" cy="-18" rx="40.89" ry="18"/>
<text text-anchor="middle" x="373.95" y="-14.3" font-family="Times,serif" font-size="14.00">MF&#45;27</text>
</g>
<!-- 1002&#45;&gt;1127 -->
<g id="FR&#45;2&#45;&#45;MF&#45;27" class="edge">
<title>1002&#45;&gt;1127</title>
<path fill="none" stroke="#222222" d="M373.95,-71.7C373.95,-63.98 373.95,-54.71 373.95,-46.11"/>
<polygon fill="#222222" stroke="#222222" points="377.45,-46.1 373.95,-36.1 370.45,-46.1 377.45,-46.1"/>
</g>
<!-- 1115 -->
<g id="MF&#45;15" class="node">
<title>1115</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="440.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="440.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;15</text>
</g>
<!-- 1109 -->
<g id="MF&#45;9" class="node">
<title>1109</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="534.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="534.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;9</text>
</g>
<!-- 1123 -->
<g id="MF&#45;23" class="node">
<title>1123</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="628.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="628.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;23</text>
</g>
<!-- 1103 -->
<g id="MF&#45;3" class="node">
<title>1103</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="722.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="722.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;3</text>
</g>
<!-- 1107 -->
<g id="MF&#45;7" class="node">
<title>1107</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="810.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="810.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;7</text>
</g>
<!-- 1112 -->
<g id="MF&#45;12" class="node">
<title>1112</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="240.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="240.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;12</text>
</g>
<!-- 1112&#45;&gt;1001 -->
<g id="MF&#45;12&#45;&#45;FR&#45;1" class="edge">
<title>1112&#45;&gt;1001</title>
<path fill="none" stroke="#222222" d="M240.95,-215.7C240.95,-207.98 240.95,-198.71 240.95,-190.11"/>
<polygon fill="#222222" stroke="#222222" points="244.45,-190.1 240.95,-180.1 237.45,-190.1 244.45,-190.1"/>
</g>
<!-- 1101 -->
<g id="MF&#45;1" class="node">
<title>1101</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="326.95" cy="-162" rx="35.19" ry="18"/>
<text text-anchor="middle" x="326.95" y="-158.3" font-family="Times,serif" font-size="14.00">MF&#45;1</text>
</g>
<!-- 1101&#45;&gt;1002 -->
<g id="MF&#45;1&#45;&#45;FR&#45;2" class="edge">
<title>1101&#45;&gt;1002</title>
<path fill="none" stroke="#222222" d="M337.85,-144.76C343.66,-136.11 350.93,-125.27 357.43,-115.6"/>
<polygon fill="#222222" stroke="#222222" points="360.44,-117.4 363.1,-107.15 354.62,-113.5 360.44,-117.4"/>
</g>
<!-- 1116 -->
<g id="MF&#45;16" class="node">
<title>1116</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="904.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="904.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;16</text>
</g>
<!-- 1120 -->
<g id="MF&#45;20" class="node">
<title>1120</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1004.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1004.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;20</text>
</g>
<!-- 1105 -->
<g id="MF&#45;5" class="node">
<title>1105</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1098.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="1098.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;5</text>
</g>
<!-- 1110 -->
<g id="MF&#45;10" class="node">
<title>1110</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1192.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1192.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;10</text>
</g>
<!-- 1119 -->
<g id="MF&#45;19" class="node">
<title>1119</title>
<ellipse fill="#dddddd" stroke="#777777" cx="340.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="340.95" y="-230.3" font-family="Times,serif" font-size="14.00" fill="#666666">MF&#45;19</text>
</g>
<!-- 1119&#45;&gt;1001 -->
<g id="MF&#45;19&#45;&#45;FR&#45;1" class="edge">
<title>1119&#45;&gt;1001</title>
<path fill="none" stroke="#bbbbbb" stroke-dasharray="5,2" d="M319.74,-218.15C304.94,-207.79 285.03,-193.86 268.92,-182.58"/>
<polygon fill="#bbbbbb" stroke="#bbbbbb" points="270.63,-179.5 260.43,-176.64 266.61,-185.24 270.63,-179.5"/>
</g>
<!-- 1104 -->
<g id="MF&#45;4" class="node">
<title>1104</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1286.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="1286.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;4</text>
</g>
<!-- 1128 -->
<g id="MF&#45;28" class="node">
<title>1128</title>
<ellipse fill="#dddddd" stroke="#777777" cx="40.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="40.95" y="-230.3" font-family="Times,serif" font-size="14.00" fill="#666666">MF&#45;28</text>
</g>
<!-- 1128&#45;&gt;1201 -->
<g id="MF&#45;28&#45;&#45;CO&#45;1" class="edge">
<title>1128&#45;&gt;1201</title>
<path fill="none" stroke="#bbbbbb" stroke-dasharray="5,2" d="M62.15,-218.15C76.86,-207.86 96.61,-194.04 112.65,-182.81"/>
<polygon fill="#bbbbbb" stroke="#bbbbbb" points="114.94,-185.47 121.13,-176.87 110.93,-179.74 114.94,-185.47"/>
</g>
<!-- 1106 -->
<g id="MF&#45;6" class="node">
<title>1106</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1374.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="1374.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;6</text>
</g>
<!-- 1121 -->
<g id="MF&#45;21" class="node">
<title>1121</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1468.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1468.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;21</text>
</g>
<!-- 1111 -->
<g id="MF&#45;11" class="node">
<title>1111</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1568.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1568.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;11</text>
</g>
<!-- 1118 -->
<g id="MF&#45;18" class="node">
<title>1118</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1668.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1668.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;18</text>
</g>
<!-- 1114 -->
<g id="MF&#45;14" class="node">
<title>1114</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1768.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1768.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;14</text>
</g>
<!-- 1125 -->
<g id="MF&#45;25" class="node">
<title>1125</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="140.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="140.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;25</text>
</g>
<!-- 1125&#45;&gt;1201 -->
<g id="MF&#45;25&#45;&#45;CO&#45;1" class="edge">
<title>1125&#45;&gt;1201</title>
<path fill="none" stroke="#bbbbbb" stroke-dasharray="5,2" d="M140.95,-215.7C140.95,-207.98 140.95,-198.71 140.95,-190.11"/>
<polygon fill="#bbbbbb" stroke="#bbbbbb" points="144.45,-190.1 140.95,-180.1 137.45,-190.1 144.45,-190.1"/>
</g>
<!-- 1124 -->
<g id="MF&#45;24" class="node">
<title>1124</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1868.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1868.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;24</text>
</g>
<!-- 1117 -->
<g id="MF&#45;17" class="node">
<title>1117</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="1968.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="1968.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;17</text>
</g>
<!-- 1122 -->
<g id="MF&#45;22" class="node">
<title>1122</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="420.95" cy="-162" rx="40.89" ry="18"/>
<text text-anchor="middle" x="420.95" y="-158.3" font-family="Times,serif" font-size="14.00">MF&#45;22</text>
</g>
<!-- 1122&#45;&gt;1002 -->
<g id="MF&#45;22&#45;&#45;FR&#45;2" class="edge">
<title>1122&#45;&gt;1002</title>
<path fill="none" stroke="#222222" d="M409.81,-144.41C404.07,-135.87 396.96,-125.28 390.59,-115.79"/>
<polygon fill="#222222" stroke="#222222" points="393.48,-113.82 385,-107.47 387.67,-117.72 393.48,-113.82"/>
</g>
<!-- 1113 -->
<g id="MF&#45;13" class="node">
<title>1113</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="520.95" cy="-162" rx="40.89" ry="18"/>
<text text-anchor="middle" x="520.95" y="-158.3" font-family="Times,serif" font-size="14.00">MF&#45;13</text>
</g>
<!-- 1113&#45;&gt;1002 -->
<g id="MF&#45;13&#45;&#45;FR&#45;2" class="edge">
<title>1113&#45;&gt;1002</title>
<path fill="none" stroke="#222222" d="M493.98,-148.16C469.48,-136.49 433.24,-119.24 407,-106.74"/>
<polygon fill="#222222" stroke="#222222" points="408.35,-103.51 397.82,-102.37 405.34,-109.83 408.35,-103.51"/>
</g>
<!-- 1102 -->
<g id="MF&#45;2" class="node">
<title>1102</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="2062.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="2062.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;2</text>
</g>
<!-- 1126 -->
<g id="MF&#45;26" class="node">
<title>1126</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="2156.95" cy="-234" rx="40.89" ry="18"/>
<text text-anchor="middle" x="2156.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;26</text>
</g>
<!-- 1108 -->
<g id="MF&#45;8" class="node">
<title>1108</title>
<ellipse fill="#93d5ba" stroke="#222222" cx="2250.95" cy="-234" rx="35.19" ry="18"/>
<text text-anchor="middle" x="2250.95" y="-230.3" font-family="Times,serif" font-size="14.00">MF&#45;8</text>
</g>
</g>
</svg>
    """
  }
}
