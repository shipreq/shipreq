package shipreq.webapp.client.app.ui.reqtable

/*
//    val DeletableRcgTrie = new MTrie.Types[ReqCode.Node, ReqCode.ActiveGroup]
//
//    var deletableRcgTrie = DeletableRcgTrie.empty
//    var deletableRcgIds = UnivEq.emptySet[ReqCodeId]

//    val processReqCode: ReqCode.Value => Unit = {
//      var seen = UnivEq.emptyMutableSet[ReqCode.Value]
//
////      @tailrec
//      def go(code: ReqCode.Value): Unit = {
//        if (!seen.contains(code)) {
////println(">>>> " + code + " = " + rc.get(code))
//          seen += code
//
//          rc.get(code) foreach {
//            case a: ReqCode.ActiveGroup =>
//              deletableRcgTrie = deletableRcgTrie.put(code, a)
//              deletableRcgIds += a.id
//
//              // Detect child groups that could be deleted if this is deleted
//              val sub = rc.trie.dropPath(code)
//              sub.foreachPathAndValue((p, _) => go(code ++ p))
//              //for ((k,v) <- sub)
////              def detectKids(path: ReqCode.Value, sub: ReqCode.Trie) =
//
//            case _ => ()
//          }
//
//
//          NonEmptyVector.option(code.init) match {
//            case Some(parent) => go(parent)
//            case None => ()
//          }
//        }
//      }
//
//      c => {
//        go(c)
//      }
//    }

//    for (id <- directlySelectedRcgs)
//      processReqCode(rc reqCode id)


//    var selectedRcgs = directlySelectedRcgs

//    def liveGivenStateRC(id: ReqCodeId): Live =
//      rc.reqCode(id) match {}

    /*
    def asdf(t: ReqCode.Trie, prc: ReqCode.Trie): DeletableRCGs =
      t.map { case (n, tn) =>

        val pn = prc.get(n).fold(ReqCode.Trie.empty)(_.fold(_.next, Map.empty))

        val sub =
          tn.fold(
            b => Some(asdf(b.next, pn)),
            _ => None)

        def z = UnivEq.emptySet[ReqCodeId]

        val inChildren = sub.fold(z)(_.valuesIterator.foldLeft(z)((q, i) => i.foldValue(q, q | _)))

        val activeSubIds = pn.allValues.flatMap(_.activeId.toStream).filterNot(inChildren.contains).toSet

        val xv = DeletableRCGs.Value(activeSubIds)

        val xx = sub match {
          case Some(b) => DeletableRCGs.Branch(Some(xv), b)
          case None    => xv
        }

        (n, xx)
      }
    val omg = asdf(deletableRCGs, rc.trie)
    */

//    println(s"deletableRcgIds = $deletableRcgIds")
//    println(s"deletableRcgTrie:" + deletableRcgTrie.flatStream.map(t => "\n  " + PlainText.reqCode(t._1) + ": " + t._2).mkString(""))
    /*
    def asdf2(pathPrefix: Vector[ReqCode.Node], deletable: DeletableRcgTrie.Trie, all: ReqCode.Trie): (Vector[RcgRow], Boolean) = {

println()
println(s"asdf2: ${pathPrefix.map(_.value).mkString(".")}")
println(s"deletable: $deletable")
//      @tailrec
      def go(queue: List[DeletableRcgTrie.Entry], acc: Vector[RcgRow]): (Vector[RcgRow], Boolean) =
        queue match {
          case (curNode, curTrieNode) :: t =>
            println()
            println(s"go: ${curNode.value} -> $curTrieNode")
//            println(s"all = $all")
            //println(s"get = ${all.get(curNode)}")

            val allSubTrie = all.get(curNode).fold(ReqCode.Trie.empty)(_.fold(_.next, _ => Map.empty))
            //println(s"ast = $allSubTrie")

            def row(g: ReqCode.ActiveGroup): Option[RcgRow] = {
              val curPathPrefix = NonEmptyVector.end(pathPrefix, curNode)

              // This could be smarter by reusing results of children
              var allow = true
              var subReqs = Set.newBuilder[(ReqId, String)]
              var subGrps = Set.newBuilder[(ReqCodeId, String)]

              allSubTrie.foreachPathAndValue { (path0, data) =>
                if (allow) {
                  def code = PlainText.reqCode(curPathPrefix ++ path0)
                  //println(s"$code -- $curNode -- $pathPrefix -- $path0")
                  data match {
                    case a: ReqCode.ActiveReq =>
                      if (deletableReqs contains a.reqId)
                        subReqs += ((a.reqId, code))
                      else
                        allow = false
                    case a: ReqCode.ActiveGroup =>
                      if (deletableRcgIds contains a.id)
                        subGrps += ((a.id, code))
                      else
                        allow = false
                    case _: ReqCode.Inactive => ()
                  }
                  if (!allow)
                    println(s"  !allow - ${PlainText reqCode curPathPrefix} < $data")
                }
              }


              if (allow) {
                val codeStr = PlainText reqCode curPathPrefix
                Some(RcgRow(g.group, codeStr, subReqs.result(), subGrps.result()))
              } else
                None
            }

            val subxxx =
              curTrieNode.fold(
                b => asdf2(pathPrefix :+ curNode, b.next, allSubTrie),
                _ => (Vector.empty, true))

            val acc2 =
              if (subxxx._2)
                curTrieNode.foldValue(
                  (acc, true),
                  v => row(v).fold((acc, false))(r => (acc :+ r, true)))
              else
                (acc, true)

            val (x,y) = go(t, acc2._1 ++ subxxx._1)
            (x, y && acc2._2)

          case Nil => (acc, true)
        }

      go(deletable.toList.sortBy(_._1), Vector.empty)
    }
    val rcgRows = asdf2(Vector.empty, deletableRcgTrie, rc.trie)._1
*/

    /*
    def buildRcgRows(pathPrefix: Vector[ReqCode.Node], deletable: DeletableRcgTrie.Trie, all: ReqCode.Trie,
                     parentHasOutOfScopeLive: Boolean): Vector[RcgRow] = {
      def go(queue: List[DeletableRcgTrie.Entry], acc: Vector[RcgRow]): Vector[RcgRow] =
        queue match {
          case (curNode, curTrieNode) :: t =>
            acc
          case Nil => acc
        }
      go(deletable.toList.sortBy(_._1), Vector.empty)
    }

    val rcgRows = buildRcgRows(Vector.empty, deletableRcgTrie, rc.trie, false)
    */

//    var selectedRCGs = directlySelectedRcgs
//    for (r <- rcgRows)
//      if (!selectedRCGs.contains(r.id) && r.liveSubs().isEmpty)

    // Decide which implied reqs to recommend cascading deletion
    // (I'm sure there's a smarter way but this will do)
    /*
    changed = true
    val liveGivenStateById: ReqId => Live = id => liveGivenState(project.reqs.req(id))
    val grplive: ReqCodeId => Live = id => (Dead <~ selectedRCGs.contains(id)) //&& rc.lookup(id).
    while (changed) {
      changed = false
      for (r <- rcgRows)
        if (!selectedRCGs.contains(r.id) && r.liveSubs(liveGivenStateById, grplive).isEmpty) {
          changed = true
          selectedRCGs += r.id
        }
    }
    */


 */