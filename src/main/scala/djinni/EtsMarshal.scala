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

class EtsMarshal(spec: Spec) extends Marshal(spec) {

  override def typename(tm: MExpr): String = toEtsType(tm)
  def typename(name: String, ty: TypeDef): String = {
    val _ = ty
    idEts.ty(name)
  }

  override def fqTypename(tm: MExpr): String = typename(tm)
  def fqTypename(name: String, ty: TypeDef): String = typename(name, ty)

  override def paramType(tm: MExpr): String = toEtsType(tm)
  override def fqParamType(tm: MExpr): String = paramType(tm)

  override def returnType(ret: Option[TypeRef]): String =
    ret.fold("void")(ty => toEtsType(ty.resolved))
  override def fqReturnType(ret: Option[TypeRef]): String = returnType(ret)

  override def fieldType(tm: MExpr): String = toEtsType(tm)
  override def fqFieldType(tm: MExpr): String = fieldType(tm)

  override def toCpp(tm: MExpr, expr: String): String =
    throw new AssertionError("direct ets to cpp conversion not possible")
  override def fromCpp(tm: MExpr, expr: String): String =
    throw new AssertionError("direct cpp to ets conversion not possible")

  def nullityAnnotation(ty: Option[TypeRef]): Option[String] = None
  def nullityAnnotation(ty: TypeRef): Option[String] = None

  def references(m: Meta): Seq[SymbolReference] = Nil

  private def toEtsType(tm: MExpr): String = tm.base match {
    case MOptional =>
      assert(tm.args.size == 1)
      s"${toEtsType(tm.args.head)} | undefined"
    case MList =>
      assert(tm.args.size == 1)
      s"Array<${toEtsType(tm.args.head)}>"
    case MSet =>
      assert(tm.args.size == 1)
      s"Set<${toEtsType(tm.args.head)}>"
    case MMap =>
      assert(tm.args.size == 2)
      s"Map<${toEtsType(tm.args.head)}, ${toEtsType(tm.args(1))}>"
    case MString => "string"
    case MBinary => "ArrayBuffer | Uint8Array"
    case MDate   => "Date"
    case p: MPrimitive =>
      p.idlName match {
        case "bool" => "boolean"
        case "i64"  => "bigint"
        case _      => "number"
      }
    case d: MDef   => idEts.ty(d.name)
    case p: MParam => idEts.typeParam(p.name)
    case e: MExtern =>
      e.ets.typename.getOrElse(e.name)
  }
}
