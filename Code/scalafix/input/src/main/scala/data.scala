package shipreq {
  package webapp {
    package base {
      package data {

        object CustomField {

          object Text {
            final case class Id(value: Int) {
              val some = this
            }
          }

          object Tag {
            final case class Id(value: Int) {
              val some = this
            }
          }

        }

      }
    }
  }
}
