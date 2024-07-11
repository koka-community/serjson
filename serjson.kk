module serjson

// A very simple JSON serializer and deserializer
//
// Currently it's not a general-purpose JSON parser and has limitations,
// including at least:
// - it uses an assoc-list to represent objects, and 
//   using pattern matching during parsing will mean that changing 
//   the order of keys in an object assoc-list will break things
// - it doesn't currently handle numbers other than integers
// - it doesn't currently handle some escaped characters in strings
// - errors are almost completely uninformative
//
// It can serialize and deserialize nested structures to/from JSON with the 
// provision of two functions:
//   vfn<t> = (t) -> div serjson/value
//   pfn<t> = (serjson/value) -> <div,parse> t
// 
// vfn takes a t and returns a serjson/value, while pfn takes a serjson/value
// and returns a t (or fails the parse)
//
// given those functions, provided as implicit values, a value can be serialized
// with:
//   v.json 
// and deserialized with:
//   s.dejson

import std/core/either
import std/core/list
import std/core/string
import std/core/undiv
import std/text/parse 

pub struct object-entry {
  key: string
  value: value
}

pub type value
  String(s: string)
  Int(i: int)
  Bool(b: bool)
  Null
  Array(a: list<value>)
  Object(o: list<object-entry>)

fun object-entry/show'(e: object-entry) : div string
  "\"" ++ e.key ++ "\": " ++ e.value.show'

// quote a char for JSON
fun quote-char(c: char): string
  match c
    '"' -> "\\\""
    '\\' -> "\\\\"
    // '/' -> "\\/"
    // 'b' -> "\\b"
    // 'f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    // 'u' -> "\\u"
    _ -> c.string

// quote " and \ characters in a string
fun quote-string(s: string): string
  s.list.map(quote-char).join

fun value/show'(v: value) : div string
  match v
    String(s) -> "\"" ++ s.quote-string ++ "\""
    Int(i) -> i.show
    Bool(b) -> if b then "true" else "false"
    Null -> "null"
    Array(a) -> "[" ++ a.map(value/show').join(", ") ++ "]"
    Object(o) -> "{" ++ o.map(object-entry/show').join(", ") ++ "}"

pub fun value/show(v: value) : total string
  with pretend-no-div
  value/show'(v)

pub fun value/(==)(a: value, b: value): bool
  match (a,b) 
    (String(aa),String(bb)) -> aa == bb
    (Int(aa),Int(bb)) -> aa == bb
    (Bool(aa),Bool(bb)) -> aa == bb
    (Null,Null) -> True
    (Array(aa),Array(bb)) -> 
      match (aa,bb)
        ([],[]) -> True
        ([],_) -> False
        (_,[]) -> False
        (Cons(x,xs),Cons(y,ys)) -> 
          value/(==)(pretend-decreasing(x), y) && 
          value/(==)(pretend-decreasing(Array(xs)),Array(ys))
    (Object(aa),Object(bb)) -> 
      match (aa,bb)
        ([],[]) -> True
        ([],_) -> False
        (_,[]) -> False
        (Cons(x,xs),Cons(y,ys)) -> 
          x.key == y.key &&
          value/(==)(pretend-decreasing(x.value), y.value) &&
          value/(==)(pretend-decreasing(Object(xs)),Object(ys))
    _ -> False

pub fun eithervalue/(==)(a: either<string,value>, b: either<string,value>): bool
  match (a,b)
    (Left(_aa),Left(_bb)) -> False
    (Right(aa),Right(bb)) -> value/(==)(aa,bb)
    _ -> False

fun hex-digit()
  char-is("hex-digit", is-hex-digit)

fun four-hex-digits()
  count(4, hex-digit).string

fun quoted-char()
  char('\\')
  choose([
    {char('"')},
    {char('\\')},
    {char('/')},
    // {char('b')},
    // {char('f')},
    {char('n') ; '\n'},
    {char('r') ; '\r'},
    {char('t') ; '\t'},
    // {char('u')}
  ])

fun is-json-string-char(c)
  c != '"'
  && c != '\\'
  && !is-control(c)

fun json-string-char()
  parse/(||)(
    {char-is("json-string-chars", is-json-string-char).string}, 
    {quoted-char().string}
  )

fun double-quote()
  char('"')

fun json-string()
  double-quote()
  val str = many(json-string-char).join
  double-quote()
  str

fun is-json-whitespace-char(c)
  c == ' '
  || c == '\n'
  || c == '\r'
  || c == '\t'

fun json-whitespace()
  optional([], {chars-are("json-whitespace-chars", is-json-whitespace-char)}).string

fun bool-true()
  pstring("true")
  True

fun bool-false()
  pstring("false")
  False

fun bool()
  choose([
    {bool-true()},
    {bool-false()}
  ])

fun null()
  pstring("null")

fun is-whitespace-comma-char(c)
  c == ','
  || c.is-json-whitespace-char

fun whitespace-comma()
  optional([], {chars-are("whitespace-comma-chars", is-whitespace-comma-char)}).string

fun array-values()
  char('[')
  json-whitespace()
  val values = many({
    whitespace-comma()
    val v = pvalue'()
    whitespace-comma()
    v})
  json-whitespace()
  char(']')
  values

fun object-entries()
  char('{')
  json-whitespace()
  val entries = many({
    whitespace-comma() ;
    val k = json-string()
    json-whitespace()
    char(':')
    json-whitespace()
    val v = pvalue'()
    whitespace-comma()
    Object-entry(k, v)
  })
  json-whitespace()
  char('}')
  entries

fun serjson/pvalue'()
  choose([
    {json-string().String},
    {pint().Int},  
    {bool().Bool},
    {null() ; Null},
    {array-values().Array},
    {object-entries().Object}
  ])

pub fun serjson/pvalue()
  with pretend-no-div
  serjson/pvalue'()

pub fun parse-value(s: string)
  s.slice.parse(serjson/pvalue).either

// functions to serialize and deserialize values

pub alias vfn<t> = (t) -> total value
pub alias pfn<t> = (value) -> parse t
// need effect-polymorphic versions to allow dismissal of the inferred div 
// effect in recursive HOF cases
pub alias pvfn<t,e> = (t) -> e value
pub alias ppfn<t,e> = (value) -> <parse|e> t

pub fun string/vfn(s: string): total serjson/value
  String(s)
pub fun string/pfn(v: serjson/value): parse string
  match v
    String(s) -> s
    _ -> fail("expected string")

pub fun int/vfn(i: int): total serjson/value
  Int(i)
pub fun int/pfn(v: serjson/value): parse int
  match v
    Int(i) -> i
    _ -> fail("expected int")

pub fun bool/vfn(b: bool): total serjson/value
  Bool(b)
pub fun bool/pfn(v: serjson/value): parse bool
  match v
    Bool(b) -> b
    _ -> fail("expected bool")

pub fun null/vfn(): total serjson/value
  Null
pub fun null/pfn(v: serjson/value): parse ()
  match v
    Null -> ()
    _ -> fail("expected null")

pub fun list/make-vfn<t,e>(elvfn: pvfn<t,e>): pvfn<list<t>,e>
  fn(l)
    Array(l.map(elvfn))
pub fun list/make-pfn<t>(elpfn: ppfn<t,e>): ppfn<list<t>,e>
  fn(v: serjson/value)
    match v
      Array(a) -> a.map(elpfn)
      _ -> fail("expected array")

pub fun maybe/make-vfn<t>(vfn: pvfn<t,e>): pvfn<maybe<t>,e>
  fn(m)
    match m
      Just(v) -> Object([Object-entry("Just", vfn(v))])
      Nothing -> Object([Object-entry("Nothing", Null)])
pub fun maybe/make-pfn<t>(pfn: ppfn<t,e>): ppfn<maybe<t>,e>
  fn(v: serjson/value)
    match v
      Object([Object-entry("Just", v)]) -> Just(pfn(v))
      Object([Object-entry("Nothing", Null)]) -> Nothing
      _ -> fail("expected maybe")

pub fun either/make-vfn<t,u>(lvfn: pvfn<t,e>, rvfn: pvfn<u,e>): pvfn<either<t,u>,e>
  fn(e)
    match e
      Left(v) -> Object([Object-entry("Left", lvfn(v))])  
      Right(v) -> Object([Object-entry("Right", rvfn(v))])
pub fun either/make-pfn<t,u>(lpfn: ppfn<t,e>, rpfn: ppfn<u,e>): ppfn<either<t,u>,e>
  fn(v: serjson/value)
    match v
      Object([Object-entry("Left", v)]) -> Left(lpfn(v))
      Object([Object-entry("Right", v)]) -> Right(rpfn(v))
      _ -> fail("expected either")
 

// convert values from and to JSON

pub fun json<t>(t: t, ?vfn: vfn<t>): total string
  ?vfn(t).show

pub fun dejson<t>(s: string, ?pfn: pfn<t>): total either<string,t>
  s.slice.parse({serjson/pvalue().?pfn}).either
