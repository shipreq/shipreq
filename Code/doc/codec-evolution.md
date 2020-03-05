This is about how to evolve codecs from 1.x to 1.y.


How to evolve binary codecs
===========================

* Create an object called `RevX` where `X` is the new point version.
  eg. create `.binary.v1.Rev3` for v1.3

* For all obsolete codecs:

  * if they can't be made to compile anymore (eg. required data unavailable)
    * comment them out
    * add a comment saying "Replaced by v1.x"
    * copy the commented-out code to the new `Rev` object

  * if they can be made to compile
    * remove the implicit flag; making them explicit-only

  * repeat for any downstream codecs that may have broken due to the above change

* For any new data types, add implicit codecs in the new `Rev` object

* For all commented-out codecs in `Rev`
  * uncomment and fix them
  * change `implicit val`s into `implicit lazy val`s so that Scala.JS doesn't include unneccesarily
    (not a problem before because non-Rev objects provided neccesary segregation)


How to evolve JSON codecs
=========================

* Create an object called `RevX` where `X` is the new point version.
  eg. create `.json.v1.Rev3` for v1.3

* For all obsolete codecs:

  * if they can't be made to compile anymore (eg. required data unavailable)

    * if you can still make use of the old v1.x data in a v1.y world ----------------------------
      * comment out the encoder
      * change a JsonCodec to just a Decoder

  * if they can be made to compile
    * remove the implicit flag; making them explicit-only

  * repeat for any downstream codecs that may have broken due to the above change

* For any new data types, add implicit codecs in the new `Rev` object

* Now take a second to consider something very important...
  JSON CODECS ARE USED FOR DB SERIALISATION

  ```
  > acks 'import .*json.v1.*' webapp-server
  webapp-server/src/main/scala/shipreq/webapp/server/db/EventSerialisation.scala
  8:import shipreq.webapp.base.protocol.json.v1.Events.EventData._
  ```

  This means that READING THE PREVIOUS VERSION CORRECTLY IS MANDATORY if you change any of the following:
    * `json.v1.BaseData`
    * `json.v1.BaseMemberData1`
    * `json.v1.Events`

* For all commented-out codecs in `Rev`
  * uncomment and fix them
  * MAKE SURE THAT THEY CAN STILL READ DATA OF THE PREVIOUS VERSION
  * change `implicit val`s into `implicit lazy val`s so that Scala.JS doesn't include unneccesarily
    (not a problem before because non-Rev objects provided neccesary segregation)

