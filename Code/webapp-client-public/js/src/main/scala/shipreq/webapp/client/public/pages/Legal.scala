package shipreq.webapp.client.public.pages

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.client.public.Styles.{legal => *}
import shipreq.webapp.client.public.spa.{Page, RouterCtl}

object Legal {

  private object Dsl {

    def page(m: TagMod*): VdomTag =
      <.article(*.cont)(m: _*)

    def header(m: TagMod*): VdomTag =
      <.header(m: _*)

    def section(header: TagMod)(body1: TagMod, bodyN: TagMod*): VdomTag =
      <.section(
        <.div(*.sectionH, header),
        (body1 :: bodyN.toList).toTagMod(<.div(*.sectionP, _)))

    def footer(m: TagMod*): VdomTag =
      <.footer(m: _*)

    def lastUpdated(attr: String, text: String): VdomTag =
      <.div("This document was last updated on ", <.time(^.dateTime := attr, text), ".")

    def mailtoSupport: VdomTag =
      <.a(^.href := WebappConfig.supportEmailAddress.mailto, "Bearded Logic")

    def bullet(header: TagMod, body: TagMod): VdomTag =
      <.li(
        <.div(*.liA, header),
        <.div(body))
  }

  import Dsl._

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val Privacy = ScalaComponent.builder[RouterCtl]("Privacy").render_P(implicit rc =>

    page(
      header(
        "This Privacy Policy governs the manner in which Bearded Logic collects, uses, maintains and discloses information collected from users (each, a \"User\") of the ",
        rc.link(Page.Home)(WebappConfig.appName),
        " website (\"Site\"). This privacy policy applies to the Site and all products and services offered by Bearded Logic."
      ),

      section("Personal identification information")(
        "We may collect personal identification information from Users in a variety of ways, including, but not limited to, when Users visit our site, register on the site, respond to a survey, and in connection with other activities, services, features or resources we make available on our Site. Users may be asked for, as appropriate, email address. Users may, however, visit our Site anonymously. We will collect personal identification information from Users only if they voluntarily submit such information to us. Users can always refuse to supply personally identification information, except that it may prevent them from engaging in certain Site related activities."
      ),

      section("Non-personal identification information")(
        "We may collect non-personal identification information about Users whenever they interact with our Site. Non-personal identification information may include the browser name, the type of computer and technical information about Users means of connection to our Site, such as the operating system and the Internet service providers utilized and other similar information."
      ),

      section("Web browser cookies")(
        "Our Site may use \"cookies\" to enhance User experience. User's web browser places cookies on their hard drive for record-keeping purposes and sometimes to track information about them. User may choose to set their web browser to refuse cookies, or to alert you when cookies are being sent. If they do so, note that some parts of the Site may not function properly."
      ),

      section("How we use collected information")(
        TagMod(
          "Bearded Logic may collect and use Users personal information for the following purposes:",
          <.ul(
            bullet(
              "To improve customer service",
              "Information you provide helps us respond to your customer service requests and support needs more efficiently."
            ),
            bullet(
              "To personalize user experience",
              "We may use information in the aggregate to understand how our Users as a group use the services and resources provided on our Site."
            ),
            bullet(
              "To improve our Site",
              "We may use feedback you provide to improve our products and services."
            ),
            bullet(
              "To run a promotion, contest, survey or other Site feature",
              "To send Users information they agreed to receive about topics we think will be of interest to them."
            ),
            bullet(
              "To send periodic emails",
              "We may use the email address to send User information and updates pertaining to the service. It may also be used to respond to their inquiries, questions, and/or other requests. If User decides to opt-in to our mailing list, they will receive emails that may include company news, updates, related product or service information, etc. If at any time the User would like to unsubscribe from receiving future emails, they may do so by contacting us via our Site."
            )
          )
        )
      ),

      section("How we protect your information")(
        "We adopt appropriate data collection, storage and processing practices and security measures to protect against unauthorized access, alteration, disclosure or destruction of your personal information, username, password, transaction information and data stored on our Site.",
        "Sensitive and private data exchange between the Site and its Users happens over a SSL secured communication channel and is encrypted and protected with digital signatures."
      ),

      section("Sharing your personal information")(
        "We do not sell, trade, or rent Users personal identification information to others. We may share generic aggregated demographic information not linked to any personal identification information regarding visitors andusers with our business partners, trusted affiliates and advertisers for the purposes outlined above. We may use third party service providers to help us operate our business and the Site or administer activities on our behalf, such as sending out newsletters or surveys. We may share your information with these third parties forthose limited purposes provided that you have given us your permission."
      ),

      section("Google Analytics")(
        "We use Google Analytics to collect information about how people use this website. The information we obtain from Google Analytics helps us understand user needs so that we can offer a better user-experience.",
        "Google Analytics uses cookies to collect information about which pages you visit, how long you are on the site, how you got there (for example from a search engine, a link, an advertisement etc.) and what you select. Information collected by the cookies (including your IP address) is transmitted to and stored by Google on servers in the United States.",
        TagMod(
          "By using this website, you consent to the processing of data about you by Google in the manner described in ",
          <.a.toNewWindow("//www.google.com/policies/privacy/")("Google's Privacy Policy"),
          " and for the purposes set out above. You can ",
          <.a.toNewWindow("//tools.google.com/dlpage/gaoptout")("opt out of Google Analytics"),
          " if you disable or refuse the cookie, or use the opt-out service provided by Google."
        )
      ),

      section("Changes to this privacy policy")(
        "Bearded Logic has the discretion to update this privacy policy at any time. When we do, we will revise the updated date at the bottom of this page. We encourage Users to frequently check this page for any changes to stay informed about how we are helping to protect the personal information we collect. You acknowledge and agree that it is your responsibility to review this privacy policy periodically and become aware of modifications."
      ),

      section("Your acceptance of these terms")(
        TagMod("By using this Site, you signify your acceptance of this policy and the ", rc.link(Page.TermsOfService)("terms of service"), ".")
      ),

      section("Contacting us")(
        TagMod("If you have any questions about this Privacy Policy, the practices of this site, or your dealings with this site, please contact us at ", mailtoSupport, ".")
      ),

      footer(
        lastUpdated("2014-01-15", "15 January, 2014"),
        <.div(*.generatedBy, "Privacy policy originally created by ", <.a.toNewWindow("http://www.generateprivacypolicy.com")("Generate Privacy Policy"), ".")
      )
    )
  ).build

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val TermsOfService = ScalaComponent.builder[RouterCtl]("ToS").render_P(implicit rc =>

    page(
      header(
        "Your access to and use of this website and service (collectively the \"Service\") is conditioned on your acceptance of and compliance with these terms. These terms apply to all visitors, users and others who access or use the Service."),

      section("Beta")(
        "This Service is currently in the beta-stage of its development. Consequentially, no warranties, guarantees, or support is provided."
      ),

      section("Warranty and Liability")(
        "YOUR USE OF THIS SERVICE IS AT YOUR SOLE RISK.THIS SERVICE IS PROVIDED \"AS IS,\" \"WITH ALL FAULTS\" AND \"AS AVAILABLE\" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE."
      ),

      section("Ownership of Intellectual Property")(
        "We claim no intellectual property rights over the information and content you provide as part of the Service. Any and all requirements data you upload to the service remain 100% your property.",
        "Any information or input provided by you regarding the Service (including but not limited to user feedback, suggestions, feedback, surveys, questionnaires) will become the property of Bearded Logic and you hereby assign all rights in such information or feedback to Bearded Logic. We may use said information or input for our business or other purposes, including without limitation improvements to the Service."
      ),

      section("Changes to these terms")(
        "Bearded Logic has the discretion to update these terms at any time. When we do, we will revise the updated date at the bottom of this page. You acknowledge and agree that it is your responsibility to review these terms periodically and become aware of modifications."
      ),

      section("Your acceptance of these terms")(
        TagMod("By using this Service, you signify your acceptance of these terms and the ", rc.link(Page.Privacy)("privacy policy"), ".")
      ),

      section("Contacting us")(
        TagMod("If you have any questions about these terms, the practices of this site, or your dealings with this Service, please contact us at ", mailtoSupport, ".")
      ),

      footer(
        lastUpdated("2014-01-15", "15 January, 2014")
      )
    )
  ).build

}
