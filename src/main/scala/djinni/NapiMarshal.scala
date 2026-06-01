/** Copyright 2014 Dropbox, Inc.
  *
  * Licensed under the Apache License, Version 2.0 (the "License"); you may not
  * use this file except in compliance with the License. You may obtain a copy
  * of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  * License for the specific language governing permissions and limitations
  * under the License.
  */

package djinni

import djinni.ast._
import djinni.generatorTools._
import djinni.meta._

class NapiMarshal(spec: Spec) extends Marshal(spec) {
  final val napiBaseLibIncludePrefix = "djinni/napi/"

  override def typename(tm: MExpr): String = "napi_value"
  def typename(name: String, ty: TypeDef): String = {
    val _ = (name, ty)
    "napi_value"
  }

  override def fqTypename(tm: MExpr): String = typename(tm)
  def fqTypename(name: String, ty: TypeDef): String = typename(name, ty)

  override def paramType(tm: MExpr): String = "napi_value"
  override def fqParamType(tm: MExpr): String = paramType(tm)

  override def returnType(ret: Option[TypeRef]): String =
    ret.fold("void")(_ => "napi_value")
  override def fqReturnType(ret: Option[TypeRef]): String = returnType(ret)

  override def fieldType(tm: MExpr): String = paramType(tm)
  override def fqFieldType(tm: MExpr): String = fqParamType(tm)

  override def toCpp(tm: MExpr, expr: String): String =
    s"${helperClass(tm)}::toCpp(env, $expr)"
  override def fromCpp(tm: MExpr, expr: String): String =
    s"${helperClass(tm)}::fromCpp(env, $expr)"

  def helperClass(name: String): String = spec.napiClassIdentStyle(name)

  def helperClass(tm: MExpr): String = tm.base match {
    case d: MDef    => withNs(Some(spec.napiNamespace), helperClass(d.name))
    case e: MExtern => e.napi.translator.get
    case MOptional =>
      tm.args.head.base match {
        case d: MDef if d.defType == DInterface    => helperClass(tm.args.head)
        case e: MExtern if e.defType == DInterface => helperClass(tm.args.head)
        case _ =>
          s"::djinni::Optional<${spec.cppOptionalTemplate}, ${helperClass(tm.args.head)}>"
      }
    case MList =>
      s"::djinni::List<${helperClass(tm.args.head)}>"
    case MSet =>
      s"::djinni::Set<${helperClass(tm.args.head)}>"
    case MMap =>
      s"::djinni::Map<${helperClass(tm.args.head)}, ${helperClass(tm.args(1))}>"
    case p: MPrimitive =>
      p.idlName match {
        case "bool" => "::djinni::Bool"
        case "i64"  => "::djinni::I64"
        case _      => s"::djinni::Number<${p.cName}>"
      }
    case MString =>
      if (spec.cppUseWideStrings) "::djinni::WString" else "::djinni::String"
    case MBinary      => "::djinni::Binary"
    case MDate        => "::djinni::Date"
    case MParam(name) => spec.napiClassIdentStyle(name)
  }

  def references(m: Meta, exclude: String = ""): Seq[SymbolReference] = {
    val _ = exclude
    m match {
      case _: MOpaque =>
        List(ImportRef(q(napiBaseLibIncludePrefix + "Marshal.hpp")))
      case d: MDef    => List(ImportRef(include(d.name)))
      case e: MExtern => List(ImportRef(e.napi.header.get))
      case _          => Nil
    }
  }

  def include(ident: String): String =
    q(
      spec.napiIncludePrefix + spec.napiFileIdentStyle(
        ident
      ) + "." + spec.cppHeaderExt
    )
}
