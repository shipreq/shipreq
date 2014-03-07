package shipreq.webapp.feature.publish

import scalaz._, shipreq.webapp._, db._, lib.Types._, feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, util._
import org.joda.time.DateTime
import org.scalatest.{Matchers, FunSpec}
import net.liftweb.common.Logger
import scala.xml._

class HtmlPublisherTest extends FunSpec with Matchers {

  val ucs =
    List((UseCase.as((1:Short).tag[IsUseCaseNumber],UseCaseHeader("Delete a Use Case".tag[Validated])
      ,List(TextField(TextFieldDefinition("Description"),FieldKeyRec(10L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Description")))~>FreeText(List(PlainText("This is a link to "),DeletedRef,PlainText(".")))
        ,TextField(TextFieldDefinition("Actors"),FieldKeyRec(11L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Actors")))~>FreeText(List(PlainText("This is an invalid link to "),InvalidStepRef("1.9.9".tag[IsStepLabel]),PlainText(".")))
        ,TextField(TextFieldDefinition("Pre-Conditions"),FieldKeyRec(12L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Pre-Conditions")))~>FreeText(List(PlainText("BA is signed in and has at least one use case.\n\nClose together!\n* So what?\n  I don't care.\n* I continue.\n\n  (Blank line above)\n\n* As do I.\n\nDone. Expect a CR above and below.\n\n* OK?\nStop.")))
        ,TextField(TextFieldDefinition("Post-Conditions"),FieldKeyRec(13L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Post-Conditions")))~>FreeText(List(PlainText("* UC no longer accessable.\n* Remaining UCs have no links to the deleted UC.\n* "),MathTexTerm("\\left( \\sum_{k=1}^n a_k b_k \\right)^2 \\leq \\left( \\sum_{k=1}^n a_k^2 \\right) \\left( \\sum_{k=1}^n b_k^2 \\right)"),PlainText("\n* "),MathTexTerm("1 \\over mn"),PlainText("\n* "),MathTexTerm("c = \\sqrt{a^2+b^2}")))
        ,NormalCourseField(FieldKeyRec(14L.tag[IsFieldKeyId],FieldKeyType.NormalAndAlternateCourses,None))~>StepFieldValue(NormalCourseField(FieldKeyRec(14L.tag[IsFieldKeyId],FieldKeyType.NormalAndAlternateCourses,None)),StepTree(List(StepNode("rNqUN".tag[IsLocalStepId],0,0,List(StepNode("0Vx1T".tag[IsLocalStepId],1,1,Nil),StepNode("yHVHo".tag[IsLocalStepId],1,2,Nil),StepNode("yiPM8".tag[IsLocalStepId],1,3,Nil),StepNode("w7BGO".tag[IsLocalStepId],1,4,Nil),StepNode("yEDzS".tag[IsLocalStepId],1,5,Nil),StepNode("V1jgG".tag[IsLocalStepId],1,6,Nil),StepNode("0do4Y".tag[IsLocalStepId],1,7,Nil))))),Map("rNqUN".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("Delete a Use Case"))),None,None),"yHVHo".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("BA indicates wish to delete a UC."))),None,None),"0do4Y".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System deletes UC."))),None,None),"w7BGO".tag[IsLocalStepId]->StepText.empty,"yEDzS".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("BA confirms deletion."))),None,None),"0Vx1T".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System shows BA their UCs."))),None,None),"yiPM8".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System displays all locations where the UC is being referenced."))),None,None),"V1jgG".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System deletes the UC."))),None,None)))
        ,ExceptionCourseField(FieldKeyRec(15L.tag[IsFieldKeyId],FieldKeyType.ExceptionCourses,None))~>StepFieldValue(ExceptionCourseField(FieldKeyRec(15L.tag[IsFieldKeyId],FieldKeyType.ExceptionCourses,None)),StepTree(Nil),Map())
        ,FlowGraphField(FieldKeyRec(24L.tag[IsFieldKeyId],FieldKeyType.FlowGraph,None))~>()
        ,TextField(TextFieldDefinition("Use Case Relationships"),FieldKeyRec(16L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Use Case Relationships")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Constraints and Business Rules"),FieldKeyRec(17L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Constraints and Business Rules")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Frequency of Use"),FieldKeyRec(18L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Frequency of Use")))~>FreeText(List(PlainText("Occasional.")))
        ,TextField(TextFieldDefinition("Special Requirements"),FieldKeyRec(19L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Special Requirements")))~>FreeText.empty
        ,TextField(TextFieldDefinition("Assumptions"),FieldKeyRec(20L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Assumptions")))~>FreeText(List(PlainText("Refs to deleted UC will be unnormalised to "),DeletedRef,PlainText(" on load.")))
        ,TextField(TextFieldDefinition("Notes and Issues"),FieldKeyRec(21L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Notes and Issues")))~>FreeText(List(PlainText("The deleted UC's number will not be reclaimed.")))
      ),Name(BiMap("rNqUN".tag[IsLocalStepId]->"1.0".tag[IsStepLabel],"yHVHo".tag[IsLocalStepId]->"1.0.2".tag[IsStepLabel],"0do4Y".tag[IsLocalStepId]->"1.0.7".tag[IsStepLabel],"w7BGO".tag[IsLocalStepId]->"1.0.4".tag[IsStepLabel],"yEDzS".tag[IsLocalStepId]->"1.0.5".tag[IsStepLabel],"0Vx1T".tag[IsLocalStepId]->"1.0.1".tag[IsStepLabel],"yiPM8".tag[IsLocalStepId]->"1.0.3".tag[IsStepLabel],"V1jgG".tag[IsLocalStepId]->"1.0.6".tag[IsStepLabel]))),UseCaseRev(UseCaseIdent(1L.tag[IsUseCaseIdentId],(1:Short).tag[IsUseCaseNumber],1L.tag[IsProjectId]),16,447L.tag[IsUseCaseRevId],UseCaseHeader("Delete a Use Case".tag[Validated]),new DateTime(1383886942985L)))
      ,(UseCase.as((2:Short).tag[IsUseCaseNumber],UseCaseHeader("Reference an entire UC".tag[Validated])
        ,List(TextField(TextFieldDefinition("Description"),FieldKeyRec(10L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Description")))~>FreeText(List(PlainText("UCs often refer to other UCs.\n\nExamples:\n* \"Participants approve deal via UC-12.\"\n* \"Can lead to UC-5: View details and history of a deal.\"\n* \"Extends UC-2: View deals\"")))
          ,TextField(TextFieldDefinition("Actors"),FieldKeyRec(11L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Actors")))~>FreeText(List(PlainText("BA")))
          ,TextField(TextFieldDefinition("Pre-Conditions"),FieldKeyRec(12L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Pre-Conditions")))~>FreeText.empty
          ,TextField(TextFieldDefinition("Post-Conditions"),FieldKeyRec(13L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Post-Conditions")))~>FreeText.empty
          ,NormalCourseField(FieldKeyRec(14L.tag[IsFieldKeyId],FieldKeyType.NormalAndAlternateCourses,None))~>StepFieldValue(NormalCourseField(FieldKeyRec(14L.tag[IsFieldKeyId],FieldKeyType.NormalAndAlternateCourses,None)),StepTree(List(StepNode("BXjuC".tag[IsLocalStepId],0,0,List(StepNode("7JTHE".tag[IsLocalStepId],1,1,Nil),StepNode("7R31x".tag[IsLocalStepId],1,2,Nil),StepNode("m9Plg".tag[IsLocalStepId],1,3,Nil),StepNode("7oM4Z".tag[IsLocalStepId],1,4,Nil),StepNode("BZuJe".tag[IsLocalStepId],1,5,Nil))))),Map("7R31x".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System extracts the UC number from the reference. If the reference contains a title (eg. "),InvalidUseCaseRef((123:Short).tag[IsUseCaseNumber],Some("Blah")),PlainText(") then System ignores the title."))),None,None),"BZuJe".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System sends updated field value back to BA's screen."))),Some(FlowFromClause(Map("41oQD".tag[IsLocalStepId]->"2.E.1.1".tag[IsStepLabel]))),None),"BXjuC".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("BA references another UC."))),None,None),"7oM4Z".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System replaces the reference with a new reference that includes the UC's title.\nEg: "),InvalidUseCaseRef((123:Short).tag[IsUseCaseNumber],None),PlainText(" becomes "),InvalidUseCaseRef((123:Short).tag[IsUseCaseNumber],Some("The Title")))),None,None),"m9Plg".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System validates that a UC with a matching number exists."))),None,Some(FlowToClause(Map("0SVXy".tag[IsLocalStepId]->"2.E.1".tag[IsStepLabel])))),"7JTHE".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("BA enters "),InvalidUseCaseRef((123:Short).tag[IsUseCaseNumber],None),PlainText(" somewhere in their UC's fields."))),None,Some(FlowToClause(Map("0DG8G".tag[IsLocalStepId]->"2.E.2".tag[IsStepLabel]))))))
          ,ExceptionCourseField(FieldKeyRec(15L.tag[IsFieldKeyId],FieldKeyType.ExceptionCourses,None))~>StepFieldValue(ExceptionCourseField(FieldKeyRec(15L.tag[IsFieldKeyId],FieldKeyType.ExceptionCourses,None)),StepTree(List(StepNode("0SVXy".tag[IsLocalStepId],0,1,List(StepNode("41oQD".tag[IsLocalStepId],1,1,Nil))),StepNode("0DG8G".tag[IsLocalStepId],0,2,List(StepNode("bTHMB".tag[IsLocalStepId],1,1,Nil),StepNode("4U5Yc".tag[IsLocalStepId],1,2,Nil))))),Map("0SVXy".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("Referenced UC doesn't exist."))),Some(FlowFromClause(Map("m9Plg".tag[IsLocalStepId]->"2.0.3".tag[IsStepLabel],"bTHMB".tag[IsLocalStepId]->"2.E.2.1".tag[IsStepLabel]))),None),"41oQD".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System changes appends a question mark to the inside of the reference.\nEg: UC-123 becomes "),InvalidUseCaseRef((123:Short).tag[IsUseCaseNumber],None),PlainText("\nEg: \"UC-123: Hello\" becomes "),InvalidUseCaseRef((123:Short).tag[IsUseCaseNumber],Some("Hello?")))),None,Some(FlowToClause(Map("BZuJe".tag[IsLocalStepId]->"2.0.5".tag[IsStepLabel])))),"0DG8G".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("BA references a UC in a flow-clause."))),Some(FlowFromClause(Map("7JTHE".tag[IsLocalStepId]->"2.0.1".tag[IsStepLabel]))),None),"4U5Yc".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("Further processing via "),UseCaseRef((3:Short).tag[IsUseCaseNumber],"Reference a step in another UC"),PlainText("."))),None,None),"bTHMB".tag[IsLocalStepId]->StepText(FreeText(List(PlainText("System interprets the reference as pointing to the first step (ie. n.0) of the UC (provided the UC exists)."))),None,Some(FlowToClause(Map("0SVXy".tag[IsLocalStepId]->"2.E.1".tag[IsStepLabel]))))))
          ,FlowGraphField(FieldKeyRec(24L.tag[IsFieldKeyId],FieldKeyType.FlowGraph,None))~>()
          ,TextField(TextFieldDefinition("Use Case Relationships"),FieldKeyRec(16L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Use Case Relationships")))~>FreeText.empty
          ,TextField(TextFieldDefinition("Constraints and Business Rules"),FieldKeyRec(17L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Constraints and Business Rules")))~>FreeText(List(PlainText("References are only allowed between data in the same project.")))
          ,TextField(TextFieldDefinition("Frequency of Use"),FieldKeyRec(18L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Frequency of Use")))~>FreeText(List(PlainText("P(80%): \u2265 1 time per 10 UCs.\nP(50%): \u2265 3 times per 10 UCs.")))
          ,TextField(TextFieldDefinition("Special Requirements"),FieldKeyRec(19L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Special Requirements")))~>FreeText(List(PlainText("MAYBE: If the referenced UC doesn't already, it should get a line like \"Referenced in [UC-X: Xxxx]\" appended to its \"Use Case Relationships\" field.")))
          ,TextField(TextFieldDefinition("Assumptions"),FieldKeyRec(20L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Assumptions")))~>FreeText.empty
          ,TextField(TextFieldDefinition("Notes and Issues"),FieldKeyRec(21L.tag[IsFieldKeyId],FieldKeyType.Text,Some("Notes and Issues")))~>FreeText(List(PlainText("UC refs in flow-clauses are prohibited but UC step refs are not. (See "),UseCaseRef((3:Short).tag[IsUseCaseNumber],"Reference a step in another UC"),PlainText(")\nE.g. a user will not be able to flow to UC-9 but will be able to flow to 9.0, 9.1, etc.")))
        ),Name(BiMap("7R31x".tag[IsLocalStepId]->"2.0.2".tag[IsStepLabel],"0SVXy".tag[IsLocalStepId]->"2.E.1".tag[IsStepLabel],"41oQD".tag[IsLocalStepId]->"2.E.1.1".tag[IsStepLabel],"BZuJe".tag[IsLocalStepId]->"2.0.5".tag[IsStepLabel],"0DG8G".tag[IsLocalStepId]->"2.E.2".tag[IsStepLabel],"BXjuC".tag[IsLocalStepId]->"2.0".tag[IsStepLabel],"4U5Yc".tag[IsLocalStepId]->"2.E.2.2".tag[IsStepLabel],"7oM4Z".tag[IsLocalStepId]->"2.0.4".tag[IsStepLabel],"m9Plg".tag[IsLocalStepId]->"2.0.3".tag[IsStepLabel],"bTHMB".tag[IsLocalStepId]->"2.E.2.1".tag[IsStepLabel],"7JTHE".tag[IsLocalStepId]->"2.0.1".tag[IsStepLabel]))),UseCaseRev(UseCaseIdent(2L.tag[IsUseCaseIdentId],(2:Short).tag[IsUseCaseNumber],1L.tag[IsProjectId]),11,423L.tag[IsUseCaseRevId],UseCaseHeader("Reference an entire UC".tag[Validated]),new DateTime(1382047100917L)))
    )

  val input = new Input(None, ucs)

  val expectedHtml = (
<p class="last-updated">
  Last updated:
  <time class="showdatetime" datetime="2013-11-08T05:02:22Z"></time>
</p><nav>
  <h2>Table of Contents</h2>
  <ul class="toc">
    <li>
      <a href="#UC-1">UC-1: Delete a Use Case</a>
    </li>
    <li>
      <a href="#UC-2">UC-2: Reference an entire UC</a>
    </li>
  </ul>
</nav><main>
  <article id="UC-1">
    <header>
      <h2>UC-1: Delete a Use Case</h2>
    </header>
    <table class="fvs">
      <tbody class="fvs">
        <tr class="rev">
          <th>Revision</th>
          <td>16</td>
        </tr>
        <tr class="lastupdated">
          <th>Last Updated</th>
          <td>
            <time class="showdate" datetime="2013-11-08T05:02:22Z"></time>
          </td>
        </tr>
        <tr>
          <th>Description</th>
          <td class="fvpub">
            This is a link to
            <span class="bad ref">[DELETED]</span>
            .
          </td>
        </tr>
        <tr>
          <th>Actors</th>
          <td class="fvpub">
            This is an invalid link to
            <span class="bad ref">[1.9.9?]</span>
            .
          </td>
        </tr>
        <tr>
          <th>Pre-Conditions</th>
          <td class="fvpub">
            BA is signed in and has at least one use case.
            <br/>
            <br/>
            Close together!
            <ul>
              <li>
                So what?
                <br/>
                I don't care.
              </li>
              <li>
                I continue.
                <br/>
                <br/>
                (Blank line above)
              </li>
              <li>As do I.</li>
            </ul>
            <br/>
            Done. Expect a CR above and below.
            <br/>
            <br/>
            <ul>
              <li>OK?</li>
            </ul>
            Stop.
          </td>
        </tr>
        <tr>
          <th>Post-Conditions</th>
          <td class="fvpub">
            <ul>
              <li>UC no longer accessable.</li>
              <li>Remaining UCs have no links to the deleted UC.</li>
              <li>
                <script type="math/tex">{"\\left( \\sum_{k=1}^n a_k b_k \\right)^2 \\leq \\left( \\sum_{k=1}^n a_k^2 \\right) \\left( \\sum_{k=1}^n b_k^2 \\right)"}</script>
              </li>
              <li>
                <script type="math/tex">1 \over mn</script>
              </li>
              <li>
                <script type="math/tex">{"c = \\sqrt{a^2+b^2}"}</script>
              </li>
            </ul>
          </td>
        </tr>
        <tr>
          <th>Normal Course</th>
          <td class="steps fvpub">
            <table class="lvl-0">
              <tr id="step-1_0">
                <th>1.0.</th>
                <td>Delete a Use Case</td>
              </tr>
              <tr class="ind">
                <td colspan="2">
                  <table class="lvl-1">
                    <tr id="step-1_0_1">
                      <th>1.</th>
                      <td>System shows BA their UCs.</td>
                    </tr>
                    <tr id="step-1_0_2">
                      <th>2.</th>
                      <td>BA indicates wish to delete a UC.</td>
                    </tr>
                    <tr id="step-1_0_3">
                      <th>3.</th>
                      <td>System displays all locations where the UC is being referenced.</td>
                    </tr>
                    <tr id="step-1_0_4">
                      <th>4.</th>
                      <td></td>
                    </tr>
                    <tr id="step-1_0_5">
                      <th>5.</th>
                      <td>BA confirms deletion.</td>
                    </tr>
                    <tr id="step-1_0_6">
                      <th>6.</th>
                      <td>System deletes the UC.</td>
                    </tr>
                    <tr id="step-1_0_7">
                      <th>7.</th>
                      <td>System deletes UC.</td>
                    </tr>
                  </table>
                </td>
              </tr>
            </table>
          </td>
        </tr>
        <tr>
          <th>Alternative Courses</th>
          <td></td>
        </tr>
        <tr>
          <th>Exceptions</th>
          <td></td>
        </tr>
        <tr class="flowgraph">
          <th>Flow Graph</th>
          <td
          data-dot="digraph G{rankdir=LR;ranksep=0.28;{node[style=filled fillcolor=lawngreen shape=invhouse]&quot;1.0&quot;}{edge[weight=9] node[style=filled fillcolor=lawngreen shape=ellipse]&quot;1.0&quot;-&gt;&quot;1.0.1&quot;-&gt;&quot;1.0.2&quot;-&gt;&quot;1.0.3&quot;-&gt;&quot;1.0.4&quot;-&gt;&quot;1.0.5&quot;-&gt;&quot;1.0.6&quot;-&gt;&quot;1.0.7&quot;};S[shape=circle style=filled color=black fontsize=1 height=.3]E[shape=doublecircle style=filled color=black fontsize=1 height=.3]{edge[weight=9]S-&gt;&quot;1.0&quot;;&quot;1.0.7&quot;-&gt;E}}">
</td>
        </tr>
        <tr>
          <th>Use Case Relationships</th>
          <td class="fvpub"></td>
        </tr>
        <tr>
          <th>Constraints and Business Rules</th>
          <td class="fvpub"></td>
        </tr>
        <tr>
          <th>Frequency of Use</th>
          <td class="fvpub">Occasional.</td>
        </tr>
        <tr>
          <th>Special Requirements</th>
          <td class="fvpub"></td>
        </tr>
        <tr>
          <th>Assumptions</th>
          <td class="fvpub">
            Refs to deleted UC will be unnormalised to
            <span class="bad ref">[DELETED]</span>
            on load.
          </td>
        </tr>
        <tr>
          <th>Notes and Issues</th>
          <td class="fvpub">The deleted UC's number will not be reclaimed.</td>
        </tr>
      </tbody>
    </table>
  </article>
  <article id="UC-2">
    <header>
      <h2>UC-2: Reference an entire UC</h2>
    </header>
    <table class="fvs">
      <tbody class="fvs">
        <tr class="rev">
          <th>Revision</th>
          <td>11</td>
        </tr>
        <tr class="lastupdated">
          <th>Last Updated</th>
          <td>
            <time class="showdate" datetime="2013-10-17T21:58:20Z"></time>
          </td>
        </tr>
        <tr>
          <th>Description</th>
          <td class="fvpub">
            UCs often refer to other UCs.
            <br/>
            <br/>
            Examples:
            <ul>
              <li>&quot;Participants approve deal via UC-12.&quot;</li>
              <li>&quot;Can lead to UC-5: View details and history of a deal.&quot;</li>
              <li>&quot;Extends UC-2: View deals&quot;</li>
            </ul>
          </td>
        </tr>
        <tr>
          <th>Actors</th>
          <td class="fvpub">BA</td>
        </tr>
        <tr>
          <th>Pre-Conditions</th>
          <td class="fvpub"></td>
        </tr>
        <tr>
          <th>Post-Conditions</th>
          <td class="fvpub"></td>
        </tr>
        <tr>
          <th>Normal Course</th>
          <td class="steps fvpub">
            <table class="lvl-0">
              <tr id="step-2_0">
                <th>2.0.</th>
                <td>BA references another UC.</td>
              </tr>
              <tr class="ind">
                <td colspan="2">
                  <table class="lvl-1">
                    <tr id="step-2_0_1">
                      <th>1.</th>
                      <td>
                        BA enters
                        <span class="bad ref">[UC-123?]</span>
                        somewhere in their UC's fields.
                        <span class="flow">
                          ➡
                          <a class="step" href="#step-2_E_2">2.E.2</a>
                        </span>
                      </td>
                    </tr>
                    <tr id="step-2_0_2">
                      <th>2.</th>
                      <td>
                        System extracts the UC number from the reference. If the reference contains a title (eg.
                        <span class="bad ref">[UC-123?: Blah]</span>
                        ) then System ignores the title.
                      </td>
                    </tr>
                    <tr id="step-2_0_3">
                      <th>3.</th>
                      <td>
                        System validates that a UC with a matching number exists.
                        <span class="flow">
                          ➡
                          <a class="step" href="#step-2_E_1">2.E.1</a>
                        </span>
                      </td>
                    </tr>
                    <tr id="step-2_0_4">
                      <th>4.</th>
                      <td>
                        System replaces the reference with a new reference that includes the UC's title.
                        <br/>
                        Eg:
                        <span class="bad ref">[UC-123?]</span>
                        becomes
                        <span class="bad ref">[UC-123?: The Title]</span>
                      </td>
                    </tr>
                    <tr id="step-2_0_5">
                      <th>5.</th>
                      <td>
                        System sends updated field value back to BA's screen.
                        <span class="flow">
                          ⬅
                          <a class="step" href="#step-2_E_1_1">2.E.1.1</a>
                        </span>
                      </td>
                    </tr>
                  </table>
                </td>
              </tr>
            </table>
          </td>
        </tr>
        <tr>
          <th>Alternative Courses</th>
          <td></td>
        </tr>
        <tr>
          <th>Exceptions</th>
          <td class="steps fvpub">
            <table class="lvl-0">
              <tr id="step-2_E_1">
                <th>2.E.1.</th>
                <td>
                  Referenced UC doesn't exist.
                  <span class="flow">
                    ⬅
                    <a class="step" href="#step-2_0_3">2.0.3</a>
                    ,
                    <a class="step" href="#step-2_E_2_1">2.E.2.1</a>
                  </span>
                </td>
              </tr>
              <tr class="ind">
                <td colspan="2">
                  <table class="lvl-1">
                    <tr id="step-2_E_1_1">
                      <th>1.</th>
                      <td>
                        System changes appends a question mark to the inside of the reference.
                        <br/>
                        Eg: UC-123 becomes
                        <span class="bad ref">[UC-123?]</span>
                        <br/>
                        Eg: &quot;UC-123: Hello&quot; becomes
                        <span class="bad ref">[UC-123?: Hello?]</span>
                        <span class="flow">
                          ➡
                          <a class="step" href="#step-2_0_5">2.0.5</a>
                        </span>
                      </td>
                    </tr>
                  </table>
                </td>
              </tr>
              <tr id="step-2_E_2">
                <th>2.E.2.</th>
                <td>
                  BA references a UC in a flow-clause.
                  <span class="flow">
                    ⬅
                    <a class="step" href="#step-2_0_1">2.0.1</a>
                  </span>
                </td>
              </tr>
              <tr class="ind">
                <td colspan="2">
                  <table class="lvl-1">
                    <tr id="step-2_E_2_1">
                      <th>1.</th>
                      <td>
                        System interprets the reference as pointing to the first step (ie. n.0) of the UC (provided the UC exists).
                        <span class="flow">
                          ➡
                          <a class="step" href="#step-2_E_1">2.E.1</a>
                        </span>
                      </td>
                    </tr>
                    <tr id="step-2_E_2_2">
                      <th>2.</th>
                      <td>
                        Further processing via
                        <span class="uc outofscope">
                          UC-3
                          <sup>(Reference a step in another UC)</sup>
                        </span>
                        .
                      </td>
                    </tr>
                  </table>
                </td>
              </tr>
            </table>
          </td>
        </tr>
        <tr class="flowgraph">
          <th>Flow Graph</th>
          <td
          data-dot="digraph G{rankdir=LR;ranksep=0.28;{node[style=filled fillcolor=lawngreen shape=invhouse]&quot;2.0&quot;}{edge[weight=9] node[style=filled fillcolor=lawngreen shape=ellipse]&quot;2.0&quot;-&gt;&quot;2.0.1&quot;-&gt;&quot;2.0.2&quot;-&gt;&quot;2.0.3&quot;-&gt;&quot;2.0.4&quot;-&gt;&quot;2.0.5&quot;}{node[style=filled fillcolor=tomato shape=octagon]&quot;2.E.1&quot;-&gt;&quot;2.E.1.1&quot;;&quot;2.E.2&quot;-&gt;&quot;2.E.2.1&quot;-&gt;&quot;2.E.2.2&quot;}&quot;2.0.1&quot;-&gt;&quot;2.E.2&quot;;&quot;2.0.3&quot;-&gt;&quot;2.E.1&quot;;&quot;2.E.1.1&quot;-&gt;&quot;2.0.5&quot;;&quot;2.E.2.1&quot;-&gt;&quot;2.E.1&quot;;S[shape=circle style=filled color=black fontsize=1 height=.3]E[shape=doublecircle style=filled color=black fontsize=1 height=.3]{edge[weight=9]S-&gt;&quot;2.0&quot;;{&quot;2.0.5&quot;;&quot;2.E.2.2&quot;}-&gt;E}}">
</td>
        </tr>
        <tr>
          <th>Use Case Relationships</th>
          <td class="fvpub"></td>
        </tr>
        <tr>
          <th>Constraints and Business Rules</th>
          <td class="fvpub">References are only allowed between data in the same project.</td>
        </tr>
        <tr>
          <th>Frequency of Use</th>
          <td class="fvpub">
            P(80%): ≥ 1 time per 10 UCs.
            <br/>
            P(50%): ≥ 3 times per 10 UCs.
          </td>
        </tr>
        <tr>
          <th>Special Requirements</th>
          <td class="fvpub">
            MAYBE: If the referenced UC doesn't already, it should get a line like &quot;Referenced in [UC-X: Xxxx]&quot; appended to its &quot;Use Case Relationships&quot; field.
          </td>
        </tr>
        <tr>
          <th>Assumptions</th>
          <td class="fvpub"></td>
        </tr>
        <tr>
          <th>Notes and Issues</th>
          <td class="fvpub">
            UC refs in flow-clauses are prohibited but UC step refs are not. (See
            <span class="uc outofscope">
              UC-3
              <sup>(Reference a step in another UC)</sup>
            </span>
            )
            <br/>
            E.g. a user will not be able to flow to UC-9 but will be able to flow to 9.0, 9.1, etc.
          </td>
        </tr>
      </tbody>
    </table>
  </article>
</main>
  )

  val logger = Logger.apply("BLAH!")

  describe("HTML Publisher") {
    it("should render sample data as expected") {
      val html = HtmlPublisher.publish(input)
      //import scalaz._, Scalaz._
      //logger.debug("\n\n\n" + html.shows + "\n\n")
      //norm(html) shouldBe norm(expectedHtml)
      compareLists(norm(html), norm(expectedHtml))
    }
  }

  val pretty = new PrettyPrinter(160,2)

  def norm(html: NodeSeq): List[String] =
    pretty.format(
      XML.loadString(
        Utility.serialize(<text>{html}</text>).toString))
    .split("\n").toList

  def compareLists(as: List[String], es: List[String]): Unit = {
    for ((a,e) <- as zip es)
      a shouldBe e
    as.size shouldBe es.size
  }

  // ===================================================================================================================

  trait ValuePublisherTester {
    def testN(terms: FreeTextTerm*)(exp: NodeSeq): Unit = test(terms: _*)(exp.toString)
    def test(terms: FreeTextTerm*)(exp: String): Unit
  }

  def textFieldLike(t: ValuePublisherTester) = {
    import t._

    it("should render markup tokens") {
      test(PlainText("Blah\n\nHehe\n* LI 1\n* LI 2"))("""Blah<br/><br/>Hehe<ul><li>LI 1</li><li>LI 2</li></ul>""")
    }
    it("should render FTT: step refs") {
      testN(StepRef("ID12".tag, "1.2".tag))(<span class="wouldbelink step">[1.2]</span>)
      testN(InvalidStepRef("1.9.9".tag))   (<span class="bad ref">[1.9.9?]</span>)
      testN(DeletedRef)                    (<span class="bad ref">[DELETED]</span>)
    }
    it("should render FTT: UC refs") {
      val num = 3.toShort.tag[IsUseCaseNumber]
      testN(UseCaseRef(num, "Do Stuff"))             (<span class="wouldbelink uc">[UC-3: Do Stuff]</span>)
      testN(UseCaseSelfRef(num, "Do Stuff"))         (<span class="wouldbelink uc">[UC-3: Do Stuff]</span>)
      testN(InvalidUseCaseRef(num, Some("Do Stuff")))(<span class="bad ref">[UC-3?: Do Stuff]</span>)
      testN(InvalidUseCaseRef(num, None))            (<span class="bad ref">[UC-3?]</span>)
    }
    it("should render FTT: math") {
      testN(MathTexTerm("xxx"))(<script type="math/tex">xxx</script>)
    }
  }

  describe("Value Publisher: TextField") {
    val tester = new ValuePublisherTester {
      override def test(terms: FreeTextTerm*)(exp: String): Unit = {
        val t = FreeText(terms.toList)
        val o = HtmlFieldValuePublishers.textField(t)
        o.toString shouldBe exp
      }
    }
    it should behave like textFieldLike(tester)
  }

  describe("Value Publisher: StepField") {
    def testST(t: StepText)(exp: String): Unit = {
      val o = HtmlFieldValuePublishers.stepField(t)
      o.toString shouldBe exp
    }
    val tester = new ValuePublisherTester {
      override def test(terms: FreeTextTerm*)(exp: String): Unit = {
        testST(StepText(FreeText(terms.toList), None, None))(exp)
      }
    }
    it should behave like textFieldLike(tester)

    val mc = FreeText(PlainText("Cool") :: Nil)
    it ("should render flow refs (1 per flow)") {
      val ff = FlowFromClause(Map("qwe".tag -> "1.0.8".tag))
      val ft = FlowToClause(Map("0DG8G".tag -> "2.E.2".tag))
      val ef = <span class="flow"> ⬅ <span class="wouldbelink step">[1.0.8]</span></span>
      val et = <span class="flow"> ➡ <span class="wouldbelink step">[2.E.2]</span></span>
      testST(StepText(mc, Some(ff), Some(ft)))(s"Cool${ef}${et}")
      testST(StepText(mc, Some(ff), None))(s"Cool${ef}")
      testST(StepText(mc, None, Some(ft)))(s"Cool${et}")
    }
    it ("should render flow refs (2 per flow)") {
      val ff = FlowFromClause(Map("qwe".tag -> "1.0.8".tag, "asd".tag -> "1.0.1".tag))
      val ft = FlowToClause(Map("0DG8G".tag -> "2.E.2".tag, "0DG81".tag -> "2.E.3".tag))
      testST(StepText(mc, Some(ff), Some(ft)))(
        <xml:group>Cool<span class="flow"> ⬅ <span class="wouldbelink step">[1.0.1]</span> <span class="wouldbelink step">[1.0.8]</span></span><span class="flow"> ➡ <span class="wouldbelink step">[2.E.2]</span> <span class="wouldbelink step">[2.E.3]</span></span></xml:group>
        .toString)
    }
  }
}
