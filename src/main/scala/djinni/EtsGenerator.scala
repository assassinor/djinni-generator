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
import djinni.writer.IndentWriter

import scala.collection.mutable

class EtsGenerator(spec: Spec) extends Generator(spec) {
  private val marshal = new EtsMarshal(spec)

  override def generate(idl: Seq[TypeDecl]): Unit = {
    super.generate(idl)
    writeNativeRuntime()
    writeIndex(idl.collect { case itd: InternTypeDecl => itd })
  }

  override def generateEnum(
      origin: String,
      ident: Ident,
      doc: Doc,
      e: Enum
  ): Unit = {
    writeEtsFile(ident.name, origin, Nil) { w =>
      writeDoc(w, doc)
      w.w(s"export enum ${idEts.ty(ident)}").braced {
        val values = enumOptionsWithValues(e)
        for (((o, value), index) <- values.zipWithIndex) {
          val comma = if (index == values.size - 1) "" else ","
          w.wl(s"${idEts.enum(o.ident)} = $value$comma")
        }
      }
    }
  }

  private def enumOptionsWithValues(e: Enum): Seq[(Enum.Option, String)] = {
    val normal = normalEnumOptions(e)
    e.options.flatMap { option =>
      option.specialFlag match {
        case Some(Enum.SpecialFlag.NoFlags) => Some(option -> "0")
        case Some(Enum.SpecialFlag.AllFlags) =>
          Some(option -> ((1 << normal.size) - 1).toString)
        case Some(_) => None
        case None =>
          val index = normal.indexWhere(_.ident == option.ident)
          Some(
            option -> (if (e.flags) (1 << index).toString else index.toString)
          )
      }
    }
  }

  override def generateRecord(
      origin: String,
      ident: Ident,
      doc: Doc,
      params: Seq[TypeParam],
      r: Record
  ): Unit = {
    val refs = refsForRecord(r, ident.name)
    writeEtsFile(ident.name, origin, refs) { w =>
      writeDoc(w, doc)
      val typeParams = etsTypeParams(params)
      w.w(s"export class ${idEts.ty(ident)}$typeParams").braced {
        for (f <- r.fields) {
          writeDoc(w, f.doc)
          w.wl(s"readonly ${idEts.field(f.ident)}: ${marshal.fieldType(f.ty)}")
        }
        w.wl
        writeEtsConstants(w, idEts.ty(ident), r.consts)
        if (r.consts.nonEmpty) {
          w.wl
        }
        w.w(s"constructor(")
        w.w(
          r.fields
            .map(f => s"${idEts.local(f.ident)}: ${marshal.paramType(f.ty)}")
            .mkString(", ")
        )
        w.wl(")").braced {
          for (f <- r.fields) {
            w.wl(s"this.${idEts.field(f.ident)} = ${idEts.local(f.ident)}")
          }
        }
      }
    }
  }

  override def generateInterface(
      origin: String,
      ident: Ident,
      doc: Doc,
      typeParams: Seq[TypeParam],
      i: Interface
  ): Unit = {
    val refs = mutable.TreeSet[String]()
    i.methods.foreach { m =>
      m.params.foreach(p => collectRefs(p.ty.resolved, ident.name, refs))
      m.ret.foreach(r => collectRefs(r.resolved, ident.name, refs))
    }
    i.consts.foreach(c => collectRefs(c.ty.resolved, ident.name, refs))

    val imports =
      refs.toSeq.map(r => s"import { ${idEts.ty(r)} } from './${idEts.ty(r)}'")
    val needsNative = i.ext.cpp
    writeEtsFile(ident.name, origin, imports) { w =>
      if (needsNative) {
        w.wl("import { getNativeModule } from './DjinniNativeRuntime'")
        w.wl
        writeNativeModuleInterface(w, ident, i)
        writeNativeModuleAccessor(w, ident)
        w.wl
      }
      writeDoc(w, doc)
      val typeParamsText = etsTypeParams(typeParams)
      w.w(s"export abstract class ${idEts.ty(ident)}$typeParamsText").braced {
        if (needsNative) {
          w.wl("private readonly _djinniNativeRef?: number")
          w.wl
          w.wl("protected constructor(nativeRef?: number)").braced {
            w.wl("this._djinniNativeRef = nativeRef")
          }
          w.wl
          w.wl("_djinni_getNativeRef(): number").braced {
            w.wl("if (this._djinniNativeRef === undefined) {")
            w.nested { w.wl("""throw new Error("nativeRef is undefined")""") }
            w.wl("}")
            w.wl("return this._djinniNativeRef")
          }
          w.wl
          w.wl(
            s"static _djinni_fromNativeRef(nativeRef: number | undefined): ${idEts.ty(ident)} | undefined"
          ).braced {
            w.wl("if (nativeRef === undefined || nativeRef === null) {")
            w.nested { w.wl("return undefined") }
            w.wl("}")
            w.wl(s"return new ${idEts.ty(ident)}CppProxy(nativeRef)")
          }
          w.wl
        }

        writeEtsConstants(w, idEts.ty(ident), i.consts)
        if (i.consts.nonEmpty) {
          w.wl
        }
        for (m <- i.methods if m.static) {
          writeStaticMethod(w, ident, m)
          w.wl
        }
        for (m <- i.methods if !m.static) {
          writeDoc(w, m.doc)
          w.wl(methodSignature(m, abstractMethod = true))
        }
      }

      if (needsNative) {
        w.wl
        w.w(s"class ${idEts.ty(ident)}CppProxy extends ${idEts.ty(ident)}")
          .braced {
            w.wl("private _djinniDestroyed: boolean = false")
            w.wl
            w.wl("constructor(nativeRef: number)").braced {
              w.wl("super(nativeRef)")
            }
            w.wl
            w.wl("_djinni_private_destroy(): void").braced {
              w.wl("const destroyed = this._djinniDestroyed")
              w.wl("this._djinniDestroyed = true")
              w.wl("if (!destroyed) {")
              w.nested {
                w.wl(
                  s"nativeModule().${cppProxyDestroyCallName(ident)}(this._djinni_getNativeRef())"
                )
              }
              w.wl("}")
            }
            for (m <- i.methods if !m.static) {
              w.wl
              writeInstanceMethod(w, ident, m)
            }
          }
        w.wl
        w.w(
          s"export function _djinni_register${idEts.ty(ident)}NativeFactory(): void"
        ).braced {
          w.wl(
            s"nativeModule().${cppProxyFactoryCallName(ident)}((nativeRef: number): ${idEts
                .ty(ident)} => new ${idEts.ty(ident)}CppProxy(nativeRef))"
          )
        }
      }
    }
  }

  private def writeStaticMethod(
      w: IndentWriter,
      owner: Ident,
      m: Interface.Method
  ): Unit = {
    val ret = marshal.returnType(m.ret)
    w.w(methodSignature(m, abstractMethod = false, forceStatic = true)).braced {
      val call = nativeCallName(owner, m)
      writeReturn(w, m.ret, s"nativeModule().$call(${args(m).mkString(", ")})")
    }
  }

  private def writeInstanceMethod(
      w: IndentWriter,
      owner: Ident,
      m: Interface.Method
  ): Unit = {
    val ret = marshal.returnType(m.ret)
    val params = paramsText(m)
    w.w(s"${idEts.method(m.ident)}($params): $ret").braced {
      w.wl("if (this._djinniDestroyed) {")
      w.nested {
        w.wl("""throw new Error("trying to use a destroyed object")""")
      }
      w.wl("}")
      val callArgs = ("this._djinni_getNativeRef()" +: args(m)).mkString(", ")
      val call = nativeCallName(owner, m)
      writeReturn(w, m.ret, s"nativeModule().$call($callArgs)")
    }
  }

  private def writeReturn(
      w: IndentWriter,
      ret: Option[TypeRef],
      expr: String
  ): Unit = {
    ret match {
      case None => w.wl(s"$expr")
      case Some(ty) =>
        w.wl(s"const result = $expr")
        w.wl(s"return ${fromNative(ty.resolved, "result")}")
    }
  }

  private def writeEtsConstants(
      w: IndentWriter,
      ownerName: String,
      consts: Seq[Const]
  ): Unit = {
    for (c <- consts) {
      writeDoc(w, c.doc)
      w.w(
        s"static readonly ${idEts.const(c.ident)}: ${marshal.fieldType(c.ty)} = "
      )
      writeEtsConst(w, ownerName, c.ty, c.value)
      w.wl
    }
  }

  private def writeEtsConst(
      w: IndentWriter,
      ownerName: String,
      ty: TypeRef,
      value: Any
  ): Unit = value match {
    case l: Long if isI64(ty.resolved) => w.w(l.toString + "n")
    case l: Long                       => w.w(l.toString)
    case d: Double                     => w.w(d.toString)
    case b: Boolean                    => w.w(if (b) "true" else "false")
    case s: String                     => w.w(s)
    case e: EnumValue => w.w(s"${idEts.ty(e.ty)}.${idEts.enum(e)}")
    case v: ConstRef  => w.w(s"$ownerName.${idEts.const(v)}")
    case z: Map[_, _] =>
      val recordMdef = ty.resolved.base.asInstanceOf[MDef]
      val record = recordMdef.body.asInstanceOf[Record]
      val valueMap = z.asInstanceOf[Map[String, Any]]
      w.w(s"new ${marshal.typename(ty)}(")
      val skipFirst = SkipFirst()
      for (f <- record.fields) {
        skipFirst { w.w(", ") }
        writeEtsConst(w, ownerName, f.ty, valueMap(f.ident.name))
      }
      w.w(")")
  }

  private def isI64(tm: MExpr): Boolean = tm.base match {
    case MOptional =>
      assert(tm.args.size == 1)
      isI64(tm.args.head)
    case p: MPrimitive => p.idlName == "i64"
    case _             => false
  }

  private def methodSignature(
      m: Interface.Method,
      abstractMethod: Boolean,
      forceStatic: Boolean = false
  ): String = {
    val staticText = if (forceStatic) "static " else ""
    val abstractText = if (abstractMethod) "abstract " else ""
    s"${staticText}${abstractText}${idEts
        .method(m.ident)}(${paramsText(m)}): ${marshal.returnType(m.ret)}"
  }

  private def paramsText(m: Interface.Method): String =
    m.params
      .map(p => s"${idEts.local(p.ident)}: ${marshal.paramType(p.ty)}")
      .mkString(", ")

  private def args(m: Interface.Method): Seq[String] =
    m.params.map(p => idEts.local(p.ident))

  private def writeNativeModuleInterface(
      w: IndentWriter,
      owner: Ident,
      i: Interface
  ): Unit = {
    w.w(s"interface Native${idEts.ty(owner)}Module").braced {
      for (m <- i.methods) {
        val nativeParams =
          (if (m.static) Seq.empty else Seq("nativeRef: number")) ++
            m.params
              .map(p => s"${idEts.local(p.ident)}: ${marshal.paramType(p.ty)}")
        w.wl(s"${nativeCallName(owner, m)}(${nativeParams
            .mkString(", ")}): ${nativeReturnType(m.ret)}")
      }
      w.wl(s"${cppProxyDestroyCallName(owner)}(nativeRef: number): void")
      w.wl(
        s"${cppProxyFactoryCallName(owner)}(factory: (nativeRef: number) => ${idEts.ty(owner)}): void"
      )
    }
    w.wl
  }

  private def writeNativeModuleAccessor(w: IndentWriter, owner: Ident): Unit = {
    val moduleType = s"Native${idEts.ty(owner)}Module"
    w.wl(s"let _djinniNativeModule: $moduleType | undefined")
    w.wl
    w.w(s"function nativeModule(): $moduleType").braced {
      w.wl("if (_djinniNativeModule === undefined) {")
      w.nested {
        w.wl(s"_djinniNativeModule = getNativeModule() as $moduleType")
      }
      w.wl("}")
      w.wl("return _djinniNativeModule")
    }
  }

  private def nativeReturnType(ret: Option[TypeRef]): String =
    ret.fold("void")(ty => nativeType(ty.resolved))

  private def nativeType(tm: MExpr): String = tm.base match {
    case MOptional =>
      assert(tm.args.size == 1)
      s"${nativeType(tm.args.head)} | undefined"
    case d: MDef
        if d.defType == DInterface && d.body.asInstanceOf[Interface].ext.cpp =>
      "number"
    case d: MDef if d.defType == DInterface => "object"
    case _                                  => marshal.fieldType(tm)
  }

  private def fromNative(tm: MExpr, expr: String): String = tm.base match {
    case MOptional =>
      assert(tm.args.size == 1)
      s"($expr === undefined || $expr === null) ? undefined : ${fromNative(tm.args.head, expr)}"
    case d: MDef
        if d.defType == DInterface && d.body.asInstanceOf[Interface].ext.cpp =>
      s"${idEts.ty(d.name)}._djinni_fromNativeRef($expr) as ${idEts.ty(d.name)}"
    case d: MDef if d.defType == DInterface =>
      s"$expr as ${idEts.ty(d.name)}"
    case _ => expr
  }

  private def nativeCallName(owner: Ident, m: Interface.Method): String =
    s"${idEts.method(owner)}${idEts.ty(m.ident)}"

  private def cppProxyFactoryCallName(owner: Ident): String =
    s"${idEts.method(owner)}RegisterCppProxyFactory"

  private def cppProxyDestroyCallName(owner: Ident): String =
    s"${idEts.method(owner)}NativeDestroy"

  private def refsForRecord(r: Record, self: String): Seq[String] = {
    val refs = mutable.TreeSet[String]()
    r.fields.foreach(f => collectRefs(f.ty.resolved, self, refs))
    r.consts.foreach(c => collectRefs(c.ty.resolved, self, refs))
    refs.toSeq.map(r => s"import { ${idEts.ty(r)} } from './${idEts.ty(r)}'")
  }

  private def collectRefs(
      tm: MExpr,
      self: String,
      refs: mutable.Set[String]
  ): Unit = {
    tm.args.foreach(collectRefs(_, self, refs))
    tm.base match {
      case d: MDef if d.name != self => refs.add(d.name)
      case _                         =>
    }
  }

  private def etsTypeParams(params: Seq[TypeParam]): String =
    if (params.isEmpty) ""
    else params.map(p => idEts.typeParam(p.ident)).mkString("<", ", ", ">")

  private def writeEtsFile(
      ident: String,
      origin: String,
      imports: Iterable[String]
  )(f: IndentWriter => Unit): Unit = {
    createFile(
      spec.etsOutFolder.get,
      idEts.ty(ident) + ".ets",
      w => {
        w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
        w.wl("// This file was generated by Djinni from " + origin)
        w.wl
        imports.toSeq.sorted.foreach(w.wl)
        if (imports.nonEmpty) {
          w.wl
        }
        f(w)
      }
    )
  }

  private def writeNativeRuntime(): Unit = {
    createFile(
      spec.etsOutFolder.get,
      "DjinniNativeRuntime.ets",
      w => {
        w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
        w.wl("// This file was generated by Djinni.")
        w.wl
        w.wl("let _djinniNativeModule: object | undefined")
        w.wl
        w.w("export function registerNativeModule(module: object): void")
          .braced {
            w.wl("if (_djinniNativeModule !== undefined) {")
            w.nested {
              w.wl("if (_djinniNativeModule !== module) {")
              w.nested {
                w.wl(
                  """throw new Error("Djinni native module is already registered")"""
                )
              }
              w.wl("}")
              w.wl("return")
            }
            w.wl("}")
            w.wl("_djinniNativeModule = module")
          }
        w.wl
        w.w("export function getNativeModule(): object").braced {
          w.wl("if (_djinniNativeModule === undefined) {")
          w.nested {
            w.wl(
              """throw new Error("Djinni native module is not registered")"""
            )
          }
          w.wl("}")
          w.wl("return _djinniNativeModule")
        }
      }
    )
  }

  private def writeIndex(idl: Seq[InternTypeDecl]): Unit = {
    for (folder <- spec.etsOutFolder) {
      createFile(
        folder,
        "index.ets",
        w => {
          val nativeInterfaces = idl.collect {
            case td if isNativeInterface(td) => idEts.ty(td.ident)
          }

          w.wl(
            "import { registerNativeModule as _djinni_setNativeModule } from './DjinniNativeRuntime'"
          )
          for (name <- nativeInterfaces) {
            w.wl(
              s"import { _djinni_register${name}NativeFactory } from './$name'"
            )
          }
          w.wl
          w.wl("let _djinniFactoriesRegistered: boolean = false")
          w.wl
          w.w("export function registerNativeModule(module: object): void")
            .braced {
              w.wl("_djinni_setNativeModule(module)")
              w.wl("if (_djinniFactoriesRegistered) {")
              w.nested {
                w.wl("return")
              }
              w.wl("}")
              w.wl("_djinniFactoriesRegistered = true")
              for (name <- nativeInterfaces) {
                w.wl(s"_djinni_register${name}NativeFactory()")
              }
            }
          w.wl
          w.wl("export { getNativeModule } from './DjinniNativeRuntime'")
          for (td <- idl) {
            val name = idEts.ty(td.ident)
            w.wl(s"export { $name } from './$name'")
          }
        }
      )
    }
  }

  private def isNativeInterface(td: InternTypeDecl): Boolean = td.body match {
    case i: Interface => i.ext.cpp
    case _            => false
  }
}
