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
//   valuefn<t> = (t) -> div serjson/value
//   pfn<t> = (serjson/value) -> <div,parse> t
// 
// valuefn takes a t and returns a serjson/value, while pfn takes a serjson/value
// and returns a t (or fails the parse)
//
// given those functions, provided as implicit values, a value can be serialized
// with:
//   v.json 
// and deserialized with:
//   s.dejson

import std/core/either
import std/core/list
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

fun value/show'(v: value) : div string
  match v
    String(s) -> "\"" ++ s ++ "\""
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

pub alias pfn<t> = (serjson/value) -> parse t
pub alias valuefn<t> = (t) -> total serjson/value

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

fun json-string-chars()
  parse/(||)(
    {chars-are("json-string-chars", is-json-string-char).string}, 
    {quoted-char().string}
  )

fun double-quote()
  char('"')

fun json-string()
  double-quote()
  val str = many(json-string-chars).join
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

pub fun dejson<t>(s: string, ?pfn: pfn<t>): total either<string,t>
  s.slice.parse({serjson/pvalue().?pfn}).either

pub fun json<t>(t: t, ?valuefn: valuefn<t>): total string
  ?valuefn(t).show
