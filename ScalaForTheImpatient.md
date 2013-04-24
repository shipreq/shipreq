Chapter 1
=========
* val x,y = 5
* val (x,y) = (5,6)
* x.unary_-() = -x

Chapter 2
=========
* 1 to 10 = [1,10]
* 1 until 10 = [1,10)
* for (i <- 1 to 3; j <- 1 to 3 if i!=j) yield i*10+j;
  // Vector(12, 13, 21, 23, 31, 32)
* Method params are named and defaultable
* Varargs. Param decl like Int*, expand into args like `1 to 5: _*`
* catch {
    case _: IOException => ...
    case e: RuntimeException => ...
  }

Chapter 3
=========
* Arrays same as Java. Fixed length, mutable elements.
* ArrayBuffer.sorted(_ < _) sorts asc
* TraversableOnce = any collection, like Java's Iterable
* Seq = any indexed collection, like array, list, string

Chapter 4
=========
* Map(k1 -> v1, k2 -> v2) // immutable
* scala.collection.mutable.{Map,HashMap}
* map(k) returns v or null
* map.get(k) returns Option[V]
* map.getOrElse(v,d)
* map(k) = v
* map += (k1 -> v1, k2 -> v2)
* for ((k,v) <- map) // using pattern matching, not tuple
* for ((k,v) <- map) yield (k,v) returns a new map
* scala.collection.immutable.SortedMap. No mutable counterpart.
* (1,"2",3) = Tuple3[Int,String,Int]
* tuple._2 returns 2nd element
* Use pattern matching with tuples. val (a,b,c) = t

Chapter 5
=========
* Omition of () from method decl prevents use of () in call site.
* Any non-decl code in class is part of primary constructor
* private[this] is instance-private rather than class-private
* @BeanProperty annotated fields get both Scala + Java style getters/setters.
* def name_=(name:String) {}
* def this(...){} = aux constructor
* Use default params in primary constructor to reduce aux constructors
* Nested class types are scoped by parent INSTANCE and considered different
  classes. To share nested classes either:
  1. Move inner class to object.
  2. Use type projection Parent#Child as the type, instead of Parent.Child.

Chapter 6
=========
* C++ style typedefs: type ID = Int
* object Hello extends App {...} // ARGV available via args
* object MyEnum extends Enumeration { val A,B,C,D = Value }
* object MyEnum extends Enumeration {
    val A = Value(1)      // set id
    val B = Value("B!")   // set name
    val C = Value(3,"C!") // set id & name
* The type is MyEnum.Value. To avoid this do:
  object MyEnum extends Enumeration {
    type MyEnum = Value
    val A,B,C,D = Value
  }
  So that import MyEnum._ includes the typedef MyEnum as well as values.
* MyEnum.values = collection
* MyEnum(0)            // by id
* MyEnum.withName("A") // by name

Chapter 7
=========
* Auto import: java.lang, scala, Predef
* Everything in parent package(s) is in-scope and need not be qualified.
* When multiple packages are declared at once, only the child's contents are
  automatically included in-scope.

  // Given
     com.mbl.MblShared
     com.mbl.msg.mobo.Stuff
     com.mbl.msg.tlm.util.Counter

  package com.mbl.msg.tlm
  package model

  MblShared      // FAILS: com.mbl     not in-scope
  msg.MblShared  // FAILS: com         not in-scope
  mobo.Stuff     // FAILS: com.mbl.msg not in-scope
  util.Counter   // com.mbl.msg.tlm.util is in-scope via first package decl

* Package objects are anonymous objects accessed via package name
* Classes, vars etc can be package-scoped but not limited to their own package.
  // com.beardedlogic.blah.model
  private[model] val x = 2
  private[beardedlogic] val x = 2
* imports can import partial packages to reduce qualifying later
  import com.beardedlogic._
  gametimers.util.Util.doStuff
* imports can be anywhere
* import java.awt.{Color,Font}      // multiple
* import java.awt.{Color => Colour} // rename
* import java.awt.{Color => _, _}   // exclusion

Chapter 8
=========
* isInstanceOf[T], asInstanceOf[T], classOf[T]
* Prefer i match { case ... } to above
* val can override super var and parameterless super def
* def can only override def
* var can only override abstract var
* Structural type def: A{def name:String} means anonymous class based on A with
  anon method name(). Like singleton classes in ruby. Creation same as Java.
* "override" keyword optional when super method is abstract
* AnyRef is like Java's Object
* AnyVal is like primitives (inc void/Unit)
* Any is everything except Unit
* equals - Identity equality. Can be different instances.
* eq/ne  - Reference equality. Same instance. Only for AnyRef.
* ==/!=  - Declared final in Any. Delegates to equals.

Chapter 9
=========
* scala.io.Source.fromFile(filename).mkString // file --> str
* scala.io.Source.fromFile(filename).getLines.toArray
* scala.io.Source.fromURL(url)
* scala.io.Source.fromString()
* scala.io.Source.stdin
* for (ch <- source) ...
* for (line <- source.getLines) ...
* Use Java stuff for reading binary files and writing files
* "%d + %d".format(a,b)

* import sys.process._
* val exitcode = "ls -l" !   // output appears in stdout
* val output   = "ls -l" !!
* "ls -l" #| "grep omg" !
* "ls -l" #> new File("omg") !
* "grep scala" #< new URL(...) !
* For different working dir and env vars
  "pwd" #| Process(cmd, new File(dir), ("LANG","it_IT")) !

* "\\s+".r
* """\s+""".r
* regex.findAllIn(str) // matches Ruby's scan
* regex.(find|replace)(First|All)In
* val regex = """(\d+)\s+(\S+)""".r
  val regex(qty,name) = "99 bottles" // create vals from groups

Chapter 10
==========
* class Abc extends Logged {}
  new Abc with ConsoleLogger // overrides Logged
  new Abc with FileLogger    // overrides Logged
* Sub-traits can call super-trait methods when overriding.
* To override and call an abstract super-trait method use "abstract override".
* Concrete trait fields are not part of the trait, but added to classes.
* Traits can have abstract fields.
* Traits cannot have constructor params, but can have constructor code.
* Constructor order = superclass, traits (L-R), class.
  Eg. class SavingsAccount extends Account with FileLogger and ShortLogger
  1. Account
  2. Logger
  3. FileLogger
  4. ShortLogger
  5. SavingsAccount

* Init abstract trait fields used in trait constructors by an early anon
  constructor:

    // Per-class
    class SavingsAccount extends {
      val filename = "out.log" // decl in FileLogger
    } with Account with FileLogger {
      // SavingsAccount body`
    }

    // Per-instance
    new {
      val filename = "out.log" // decl in FileLogger
    } with SimpleAccount with FileLogger

* Traits can extends classes. When mixing-in, the superclass is either or
  provided by the trait, or if already provided in the class decl, checked for
  compatibility.
    trait T extends Exception
    class A extends T                    // A -> Exception
    class B extends IOException with T   // A -> IOException
    class B extends ArrayList with T     // ERROR

* Self types control mixin rules against target type.
  trait T { this: Exception => ... }        // Can only be used in Exceptions
  trait T { this: {def log():Unit} => ... } // Require methods in target

Chapter 11
==========
* Use backticks to quote identifiers. Can have vars with names of keywords.
* Infix operators: a.op(b), a op b
* Unary operators: a.op(), a op
* Special case unary operators: a.unary_[-+~!](), [-+~!]a
* Assignment operators and operators that end in a colon, are right-associative.
  1 :: 2 :: Nil  => 1 :: (2 :: Nil) => Nil.::(2).::(1) => List(1,2)
* f(a,b)     --> f.apply(a,b)     // map(k)
* f(a,b) = c --> f.update(a,b,c)  // map(k) = v
* Object.unapply() allows
  variable assignment => val Fraction(num,denom) = f
  pattern matching    => case Fraction(num,_) => println(num)
* Object.unapply() usually returns Option[]
* Object.unapplySeq(...): Options[Seq[]] can be used for vararg pattern matching
    case Name(first, last) => ...
    case Name(first, middle, last) => ...
    case Name(first, "van", "der", last) => ...

Chapter 12
==========
* Functions are first-class citizens. Eg. val fn = (x:Int) => x+1
* <method name> + _ returns a function. Eg. val fn = onPause _
* Functions as args:
  array.map( (x:Int) => x+1 )
  array.map{ (x:Int) => x+1 }
  array map { (x:Int) => x+1 }
  array.map(meth _)
  array.map{meth _}
  array map {meth _}
  array map meth _
  array.map(fn)
  array map fn
* Func decl: (type1, ..., typeN) => resType
* Higher-order function = fn that takes and/or returns a fn.
* Parameter inference:
  array.map( (x:Int) => x+1 )
  array.map( (x) => x+1 )
  array.map( x => x+1 )
  array.map( _+1 )
* Interence with anonymous functions:
  val fn = 3 * (_:Int)
  val fn: (Int)=>Int = 3*_
* Ruby's collection.each() is .foreach()
* Closure = anon fn with access to nonlocal variables (ie. block-scoped, etc)
* Auto-Currying= def multiply(x:Int)(y:Int) = x * y
* Call-by-name notation
    Ruby : def asd(&block)            asd{ ... }
    Scala: def asd(block:=> Unit){}   asd{ ... }
* Generate while/until using two call-by-name fns.
    def until(cond:=> Boolean)(block:=> Unit) {}
    until(cond){ block }
* Call-by-name args not evaluated until invoked
* return is a hack that throws an exception which will be caught by the outer
  function. try/catch blocks will catch this.

Chapter 13
==========

Chapter 14
==========

Chapter 15
==========

Chapter 16
==========
* single xml element = scala.xml.Elem
    val e = <span/>
    val html = <html><head><title>hehe</title></head><body>blah</body></html>
* NodeBuffer -- mutable XML. items += <li>example</li>
* NodeBuffer can be implicitly converted to NodeSeq to imply immutability
* Elem.child -- collection of children
* Elem.attributes
  * attr(name) -- Gets value of attribute. eg. div.attributes("class")
      Return type is Seq[Node] or null
      Call .text to convert to string (without resolving entities like &#233;)
  * attr.get(name) returns an Option[Seq[Node]]
      div.attributes.get("class").getOrElse(Text(""))
  * for (a <- elem.attributes) ... a.key ... a.value.text
  * val map : Map[String,String] = elem.attributes.asAttrMap
* Node Interpolation: <li>{ variable }</li>
  variable turns into an Atom[T] which is converted to XML on save via
  Atom.toString()
* Nestable node interpolation: <ul>{ for (i <- items) yield <li>{i}</li> }</ul>
* Attribute interpolation: Don't quote. <img src={src} />
* Attribute interpolation: Entire attribute is removed when interpolation value
  is null or None
* Use PCData to include non-XML text in XML.
    val js = """ if (temp<0) alert("Cold") """
    val x = <script>{PCData(js)}</script>

* XPATH
  * elem \ tagName - Selects children with tag name.
    val d = <dl> <dt>T1</dt><dd>D1</dd> <dt><em>T2</em></dt><dd><em>D2</em></dd> </dl>
    d \ "dt"       // NodeSeq(<dt>T1</dt>, <dt><em>T2</em></dt>)
    d \ "_" \ "em" // NodeSeq(<em>T2</em>, <em>D2</em>)
  * elem \\ tagName - As above but recursive. Any depth.
    d \\ "em" // NodeSeq(<em>T2</em>, <em>D2</em>)
  * elem \ "@attrName" - Select attribute. Only works there is one if node with
      attribute. Even if only one attribute across multiple nodes.
  * elem \\ "@attrName" - Select attributes.

* Node pattern matching.
  case <img/>        => ... // node is img with any attributes, no children.
  case <li>{_}</li>  => ... // matches <li>a</li> but not <li>a<em>b</em></li>
  case <li>{_*}</li> => ... // matches any number of children
  case <li>{children @ _*}</li> => for (c <- children) yield c // Seq[Node]
  case node @ <img/> if (node.attributes("alt").text == "TODO") => ...
* Node patterns
  * can only be Elems       // ie. ERROR: case <img/><span/> => ...
  * cannot match attributes // ie. ERROR: case <img alt="TODO" /> => ...

* Modification. Immutable so use copy with named args: label, child, attributes
    val ul = <ul>{items}</ul>
    val ol = ul.copy(label = "ol")
    val ch = ol.copy(child = ol.child ++ <li>MOAR!</li>)
* Change attribute via %
    val image = <img src="omg" />
    val copy = image % Attribtue(null, "alt", "Value of alt", Null)
    val copy = image % Attribtue(null, "alt", "Value of alt",
                       Attribtue(null, "src", "ffs", xml.Null))

* Bulk transformation.
  val rule1 = new RewriteRule {
    override def transform(n:Node) = n match {
      case e @ <ul>{_*}</ul> => e.asInstanceOf[Elem].copy(label="ol")
      case _ => n
    }
  }
  val copy = new RuleTransformer(rule1, ruleN).transform(root)

* XML.loadFile(filename)
* XML.load(inputStream)
* XML.save(filename, node)
* val str = xml.Utility.toXML(node, minimizeTags=true) // <img/> instead of <img></img>
* val str = new PrettyPrinter(width=,step=).formatNodes(nodeSeq)

* For namespaces create a NamespaceBinder and pass to Attribute(...) as scope.

Chapter 17
==========

Chapter 18
==========

Chapter 19
==========

Chapter 20
==========

Chapter 21
==========

Chapter 22
==========
