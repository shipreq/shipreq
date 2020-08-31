package shipreq.fix

import scalafix.v1._

class FixShipReq extends SemanticRule("FixShipReq") {

  override def fix(implicit doc: SemanticDocument): Patch = {
//    println("Tree.syntax\n" + doc.tree.syntax)
//    println("Tree.structure\n" + doc.tree.structure)
//    println("Tree.structureLabeled\n" + doc.tree.structureLabeled)
    Patch.empty
  }
}
