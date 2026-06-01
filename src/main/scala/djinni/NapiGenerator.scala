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

class NapiGenerator(spec: Spec) extends Generator(spec) {
  val napiMarshal = new NapiMarshal(spec)
  val cppMarshal = new CppMarshal(spec)

  def writeNapiCppFile(
      name: String,
      origin: String,
      includes: Iterable[String]
  )(f: IndentWriter => Unit): Unit =
    writeCppFileGeneric(
      spec.napiOutFolder.get,
      spec.napiNamespace,
      spec.napiFileIdentStyle,
      spec.napiIncludePrefix
    )(name, origin, includes, f)

  def writeNapiHppFile(
      name: String,
      origin: String,
      includes: Iterable[String],
      fwds: Iterable[String]
  )(f: IndentWriter => Unit): Unit =
    writeHppFileGeneric(
      spec.napiHeaderOutFolder.get,
      spec.napiNamespace,
      spec.napiFileIdentStyle
    )(name, origin, includes, fwds, f, _ => {})

  override def generate(idl: Seq[TypeDecl]): Unit = {
    super.generate(idl)
    if (spec.napiGenerateMain) {
      writeNapiMain(idl.collect { case itd: InternTypeDecl => itd })
    }
  }

  class NapiRefs(name: String) {
    var napiHpp: mutable.TreeSet[String] = mutable.TreeSet[String]()
    var napiCpp: mutable.TreeSet[String] = mutable.TreeSet[String]()

    napiHpp.add(
      "#include " + q(
        spec.napiIncludeCppPrefix + spec.cppFileIdentStyle(
          name
        ) + "." + spec.cppHeaderExt
      )
    )
    napiHpp.add(
      "#include " + q(
        napiMarshal.napiBaseLibIncludePrefix + "djinni_support.hpp"
      )
    )
    napiHpp.add(
      "#include " + q(napiMarshal.napiBaseLibIncludePrefix + "Marshal.hpp")
    )

    def find(ty: TypeRef): Unit = { find(ty.resolved) }
    def find(tm: MExpr): Unit = {
      tm.args.foreach(find)
      find(tm.base)
    }
    def find(m: Meta): Unit = for (r <- napiMarshal.references(m, name))
      r match {
        case ImportRef(arg) => napiCpp.add("#include " + arg)
        case _              =>
      }
  }

  override def generateEnum(
      origin: String,
      ident: Ident,
      doc: Doc,
      e: Enum
  ): Unit = {
    val refs = new NapiRefs(ident.name)
    val helper = napiMarshal.helperClass(ident.name)
    val cppSelf = cppMarshal.fqTypename(ident, e)
    writeNapiHppFile(ident.name, origin, refs.napiHpp, Nil) { w =>
      w.w(s"class $helper final").bracedSemi {
        w.wlOutdent("public:")
        w.wl(s"using CppType = $cppSelf;")
        w.wl("using NapiType = napi_value;")
        w.wl(s"using Boxed = $helper;")
        w.wl("static CppType toCpp(napi_env env, NapiType value);")
        w.wl("static NapiType fromCpp(napi_env env, CppType value);")
      }
    }
    writeNapiCppFile(ident.name, origin, Nil) { w =>
      w.wl(s"auto $helper::toCpp(napi_env env, NapiType value) -> CppType")
        .braced {
          w.wl("return static_cast<CppType>(::djinni::I32::toCpp(env, value));")
        }
      w.wl
      w.wl(s"auto $helper::fromCpp(napi_env env, CppType value) -> NapiType")
        .braced {
          w.wl(
            "return ::djinni::I32::fromCpp(env, static_cast<int32_t>(value));"
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
    val refs = new NapiRefs(ident.name)
    r.fields.foreach(f => refs.find(f.ty))
    val helper = napiMarshal.helperClass(ident.name)
    val cppSelf = cppMarshal.fqTypename(ident, r) + cppTypeArgs(params)
    writeNapiHppFile(ident.name, origin, refs.napiHpp, Nil) { w =>
      w.w(s"class $helper final").bracedSemi {
        w.wlOutdent("public:")
        w.wl(s"using CppType = $cppSelf;")
        w.wl("using NapiType = napi_value;")
        w.wl(s"using Boxed = $helper;")
        w.wl("static CppType toCpp(napi_env env, NapiType value);")
        w.wl("static NapiType fromCpp(napi_env env, const CppType& value);")
      }
    }
    writeNapiCppFile(ident.name, origin, refs.napiCpp) { w =>
      w.wl(s"auto $helper::toCpp(napi_env env, NapiType value) -> CppType")
        .braced {
          if (r.fields.isEmpty) {
            w.wl("(void)env;")
            w.wl("(void)value;")
            w.wl("return {};")
          } else {
            w.wl(
              "DJINNI_NAPI_ASSERT(env, value != nullptr, \"record value is null\")"
            )
            val values = r.fields.map { f =>
              val prop = idEts.field(f.ident)
              val cppValue = napiMarshal.toCpp(
                f.ty,
                s"::djinni::getProperty(env, value, ${q(prop)})"
              )
              cppValue
            }
            writeAlignedCall(
              w,
              "return CppType{",
              r.fields,
              "}",
              f => values(r.fields.indexOf(f))
            )
            w.wl(";")
          }
        }
      w.wl
      w.wl(
        s"auto $helper::fromCpp(napi_env env, const CppType& value) -> NapiType"
      ).braced {
        w.wl("napi_value object;")
        w.wl("DJINNI_NAPI_CALL(env, napi_create_object(env, &object));")
        for (f <- r.fields) {
          val prop = idEts.field(f.ident)
          val field = idCpp.field(f.ident)
          w.wl(s"::djinni::setProperty(env, object, ${q(prop)}, ${napiMarshal
              .fromCpp(f.ty, s"value.$field")});")
        }
        w.wl("return object;")
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
    val refs = new NapiRefs(ident.name)
    i.methods.foreach(m => {
      m.params.foreach(p => refs.find(p.ty))
      m.ret.foreach(refs.find)
    })
    i.consts.foreach(c => {
      refs.find(c.ty)
    })
    val helper = napiMarshal.helperClass(ident.name)
    val cppSelf = cppMarshal.fqTypename(ident, i) + cppTypeArgs(typeParams)
    writeNapiHppFile(ident.name, origin, refs.napiHpp, Nil) { w =>
      w.w(
        s"class $helper final : public ::djinni::NapiInterface<$cppSelf, $helper>"
      ).bracedSemi {
        w.wlOutdent("public:")
        w.wl(s"using Base = ::djinni::NapiInterface<$cppSelf, $helper>;")
        w.wl(s"using CppType = std::shared_ptr<$cppSelf>;")
        w.wl(s"using CppOptType = std::shared_ptr<$cppSelf>;")
        w.wl("using NapiType = napi_value;")
        w.wl(s"using Boxed = $helper;")
        w.wl("static CppType toCpp(napi_env env, NapiType value);")
        w.wl(
          "static NapiType fromCppOpt(napi_env env, const CppOptType& value);"
        )
        w.wl("static NapiType fromCpp(napi_env env, const CppType& value);")
        w.wl("static void registerNapi(napi_env env, napi_value exports);")
        w.wl(s"static constexpr bool HasCppProxy = ${if (i.ext.cpp) "true"
          else "false"};")
        w.wl(s"static constexpr bool HasEtsProxy = ${if (i.ext.ohos) "true"
          else "false"};")
        if (i.ext.cpp) {
          w.wl(
            s"static const char * cppProxyName() { return ${q(idEts.ty(ident))}; }"
          )
        }
        if (i.ext.ohos) {
          w.wl
          w.wl(
            s"class EtsProxy final : public ::djinni::EtsProxyHandle<EtsProxy>, public ::djinni::EtsProxyBase, public $cppSelf"
          ).bracedSemi {
            w.wlOutdent("public:")
            w.wl("EtsProxy(napi_env env, napi_value value, uint64_t objectId);")
            w.wl("~EtsProxy() override;")
            for (m <- i.methods) {
              val ret = cppMarshal.fqReturnType(m.ret)
              val params = m.params.map(p =>
                cppMarshal.fqParamType(p.ty) + " " + idCpp.local(p.ident)
              )
              w.wl(
                s"$ret ${idCpp.method(m.ident)}${params.mkString("(", ", ", ")")} override;"
              )
            }
          }
        }
      }
    }
    writeNapiCppFile(ident.name, origin, refs.napiCpp) { w =>
      writeInterfaceBody(w, ident, i, helper, cppSelf)
    }
  }

  private def writeInterfaceBody(
      w: IndentWriter,
      ident: Ident,
      i: Interface,
      helper: String,
      cppSelf: String
  ): Unit = {
    w.wl(s"auto $helper::toCpp(napi_env env, NapiType value) -> CppType")
      .braced {
        w.wl("return Base::_fromNapi(env, value);")
      }
    w.wl
    w.wl(
      s"auto $helper::fromCpp(napi_env env, const CppType& value) -> NapiType"
    ).braced {
      w.wl("return fromCppOpt(env, value);")
    }
    w.wl
    w.wl(
      s"auto $helper::fromCppOpt(napi_env env, const CppOptType& value) -> NapiType"
    ).braced {
      w.wl("return Base::_toNapi(env, value);")
    }
    w.wl
    if (i.ext.ohos) {
      w.wl(
        s"$helper::EtsProxy::EtsProxy(napi_env env, napi_value value, uint64_t objectId) : ::djinni::EtsProxyHandle<EtsProxy>(env, value, objectId), ::djinni::EtsProxyBase(env, value) {}"
      )
      w.wl(s"$helper::EtsProxy::~EtsProxy() = default;")
      for (m <- i.methods) {
        w.wl
        writeEtsProxyMethod(w, helper, m)
      }
      w.wl
    }
    writeNativeMethods(w, ident, i, helper, cppSelf)
    w.wl
    w.wl(s"void $helper::registerNapi(napi_env env, napi_value exports)")
      .braced {
        val methods = i.methods.filter(m => i.ext.cpp || m.static)
        if (methods.isEmpty && !i.ext.cpp) {
          w.wl("(void)env;")
          w.wl("(void)exports;")
        } else {
          w.wl("napi_property_descriptor descriptors[] = {")
          w.nested {
            if (i.ext.cpp) {
              w.wl(
                s"{ ${q(cppProxyDestroyCallName(ident))}, nullptr, ${cppProxyDestroyFunctionName(ident)}, nullptr, nullptr, nullptr, napi_default, nullptr },"
              )
              w.wl(
                s"{ ${q(cppProxyFactoryCallName(ident))}, nullptr, ${cppProxyFactoryFunctionName(ident)}, nullptr, nullptr, nullptr, napi_default, nullptr },"
              )
            }
            for (m <- methods) {
              w.wl(
                s"{ ${q(nativeCallName(ident, m))}, nullptr, ${nativeFunctionName(ident, m)}, nullptr, nullptr, nullptr, napi_default, nullptr },"
              )
            }
          }
          w.wl("};")
          w.wl(
            "DJINNI_NAPI_CALL(env, napi_define_properties(env, exports, sizeof(descriptors) / sizeof(descriptors[0]), descriptors));"
          )
        }
      }
  }

  private def writeEtsProxyMethod(
      w: IndentWriter,
      helper: String,
      m: Interface.Method
  ): Unit = {
    val ret = cppMarshal.fqReturnType(m.ret)
    val params = m.params.map(p =>
      cppMarshal.fqParamType(p.ty) + " " + idCpp.local(p.ident)
    )
    w.w(
      s"$ret $helper::EtsProxy::${idCpp.method(m.ident)}${params.mkString("(", ", ", ")")}"
    ).braced {
      m.ret match {
        case None =>
          w.wl("auto selfObject = this->objectRef();")
          w.wl("::djinni::runOnJsThreadAsync(this->env(), [=](napi_env env) {")
          w.nested {
            writeEtsProxyMethodBody(w, m, "selfObject->get(env)")
          }
          w.wl("});")
        case Some(retTy) =>
          w.wl(
            s"return ::djinni::runOnJsThreadSync(this->env(), [&](napi_env env) -> $ret {"
          )
          w.nested {
            writeEtsProxyMethodBody(w, m, "object(env)")
            w.wl(s"return ${napiMarshal.toCpp(retTy, "result")};")
          }
          w.wl("});")
      }
    }
  }

  private def writeEtsProxyMethodBody(
      w: IndentWriter,
      m: Interface.Method,
      target: String
  ): Unit = {
    for ((p, index) <- m.params.zipWithIndex) {
      w.wl(
        s"napi_value arg$index = ${napiMarshal.fromCpp(p.ty, idCpp.local(p.ident))};"
      )
    }
    val argsArray = if (m.params.isEmpty) "nullptr" else "args"
    if (m.params.nonEmpty) {
      w.wl(
        s"napi_value args[] = { ${(0 until m.params.size).map(i => s"arg$i").mkString(", ")} };"
      )
    }
    val call =
      s"::djinni::callMethod(env, $target, ${q(idEts.method(m.ident))}, ${m.params.size}, $argsArray)"
    m.ret match {
      case None    => w.wl(s"$call;")
      case Some(_) => w.wl(s"auto result = $call;")
    }
  }

  private def writeNativeMethods(
      w: IndentWriter,
      ident: Ident,
      i: Interface,
      helper: String,
      cppSelf: String
  ): Unit = {
    if (i.ext.cpp) {
      w.wl(
        s"static napi_value ${cppProxyDestroyFunctionName(ident)}(napi_env env, napi_callback_info info)"
      ).braced {
        w.wl("try").braced {
          w.wl("::djinni::NapiArgs args(env, info, 1);")
          w.wl(s"::djinni::destroyCppProxyHandle<$cppSelf>(env, args[0]);")
          w.wl("return ::djinni::undefined(env);")
        }
        w.wl("catch (...)").braced {
          w.wl("::djinni::setPendingFromCurrent(env);")
          w.wl("return ::djinni::undefined(env);")
        }
      }
      w.wl

      w.wl(
        s"static napi_value ${cppProxyFactoryFunctionName(ident)}(napi_env env, napi_callback_info info)"
      ).braced {
        w.wl("try").braced {
          w.wl("::djinni::NapiArgs args(env, info, 1);")
          w.wl(
            s"::djinni::registerCppProxyFactory(env, ${q(idEts.ty(ident))}, args[0]);"
          )
          w.wl("return ::djinni::undefined(env);")
        }
        w.wl("catch (...)").braced {
          w.wl("::djinni::setPendingFromCurrent(env);")
          w.wl("return ::djinni::undefined(env);")
        }
      }
      w.wl
    }

    for (m <- i.methods if i.ext.cpp || m.static) {
      w.wl(
        s"static napi_value ${nativeFunctionName(ident, m)}(napi_env env, napi_callback_info info)"
      ).braced {
        w.wl("try").braced {
          val argCount = m.params.size + (if (m.static) 0 else 1)
          w.wl(s"::djinni::NapiArgs args(env, info, $argCount);")
          val offset = if (m.static) 0 else 1
          if (!m.static) {
            w.wl(s"auto self = $helper::toCpp(env, args[0]);")
          }
          for ((p, index) <- m.params.zipWithIndex) {
            w.wl(s"auto ${idCpp.local(p.ident)} = ${napiMarshal
                .toCpp(p.ty, s"args[${index + offset}]")};")
          }
          val params = m.params.map(p => idCpp.local(p.ident)).mkString(", ")
          val cppCall =
            if (m.static) s"$cppSelf::${idCpp.method(m.ident)}($params)"
            else s"self->${idCpp.method(m.ident)}($params)"
          m.ret match {
            case None =>
              w.wl(s"$cppCall;")
              w.wl("return ::djinni::undefined(env);")
            case Some(retTy) =>
              w.wl(s"auto result = $cppCall;")
              w.wl(s"return ${napiMarshal.fromCpp(retTy, "result")};")
          }
        }
        w.wl("catch (...)").braced {
          w.wl("::djinni::setPendingFromCurrent(env);")
          w.wl("return ::djinni::undefined(env);")
        }
      }
      w.wl
    }
  }

  private def writeNapiMain(idl: Seq[InternTypeDecl]): Unit = {
    createFile(
      spec.napiOutFolder.get,
      "DjinniNapiMain." + spec.cppExt,
      w => {
        w.wl("// AUTOGENERATED FILE - DO NOT MODIFY!")
        w.wl("// This file provides the HarmonyOS NAPI module entry.")
        w.wl
        w.wl(
          "#include " + q(
            napiMarshal.napiBaseLibIncludePrefix + "djinni_napi_main.hpp"
          )
        )
        for (td <- idl) {
          w.wl("#include " + napiMarshal.include(td.ident.name))
        }
        w.wl
        wrapNamespace(
          w,
          spec.napiNamespace,
          w => {
            w.wl("static napi_value Init(napi_env env, napi_value exports)")
              .braced {
                w.wl("::djinni::napiInit(env);")
                for (td <- idl if td.body.isInstanceOf[Interface]) {
                  w.wl(
                    s"${napiMarshal.helperClass(td.ident.name)}::registerNapi(env, exports);"
                  )
                }
                w.wl("return exports;")
              }
          }
        )
        w.wl
        w.wl("__attribute__((constructor)) static void RegisterNapiModule()")
          .braced {
            w.wl(
              s"""::djinni::registerNapiModule("${spec.napiModuleName.get}", ${withNs(
                  Some(spec.napiNamespace),
                  "Init"
                )});"""
            )
          }
      }
    )
  }

  private def nativeCallName(owner: Ident, m: Interface.Method): String =
    s"${idEts.method(owner)}${idEts.ty(m.ident)}"

  private def cppProxyFactoryCallName(owner: Ident): String =
    s"${idEts.method(owner)}RegisterCppProxyFactory"

  private def cppProxyDestroyCallName(owner: Ident): String =
    s"${idEts.method(owner)}NativeDestroy"

  private def nativeFunctionName(owner: Ident, m: Interface.Method): String =
    s"native_${idEts.method(owner)}_${idEts.method(m.ident)}"

  private def cppProxyFactoryFunctionName(owner: Ident): String =
    s"native_${idEts.method(owner)}_registerCppProxyFactory"

  private def cppProxyDestroyFunctionName(owner: Ident): String =
    s"native_${idEts.method(owner)}_nativeDestroy"

  private def cppTypeArgs(params: Seq[TypeParam]): String =
    if (params.isEmpty) ""
    else params.map(p => idCpp.typeParam(p.ident)).mkString("<", ", ", ">")
}
