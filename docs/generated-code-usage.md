# Use Generated Code

## Java / JNI / C++ Project

### Includes & Build target
The following headers / code will be generated for each defined type:

| Type       | C++ header                 | C++ source                   | Java                | JNI header            | JNI source            |
|------------|----------------------------|------------------------------|---------------------|-----------------------|-----------------------|
| Enum/Flags | my\_enum.hpp               |                              | MyEnum.java         | NativeMyEnum.hpp      | NativeMyEnum.cpp      |
|            | my\_enum+json.hpp :two:    |                              |                     |                       |                       |
| Record     | my\_record[\_base].hpp     | my\_record[\_base].cpp :one: | MyRecord[Base].java | NativeMyRecord.hpp    | NativeMyRecord.cpp    |
|            | my\_record[\_base]+json.hpp|                              |                     |                       |                       |
|            |                       :two:|                              |                     |                       |                       |
| Interface  | my\_interface.hpp          | my\_interface.cpp :one:      | MyInterface.java    | NativeMyInterface.hpp | NativeMyInterface.cpp |

- :one: Generated only for types that contain constants.
- :two: Generated only if cpp json serialization is enabled.

Additionally `djinni_jni_main.cpp` is generated to provide a default implementation for `JNI_OnLoad` and `JNI_OnUnload`, if `--jni-generate-main=true`.

Add all generated source files to your build target, and link the C++ code against the [djinni-support-lib](https://github.com/cross-language-cpp/djinni-support-lib).

### Our JNI approach
JNI stands for [Java Native Interface](http://docs.oracle.com/javase/6/docs/technotes/guides/jni/spec/jniTOC.html), an extension of the Java language to allow interop with
native (C/C++) code or libraries.

For each type, built-in (`list`, `string`, etc.) or user-defined, Djinni produces a translator
class with a `toJava` and `fromJava` function to translate back and forth.

Application code is responsible for the initial load of the JNI library. Add a static block
somewhere in your code:

```java
class Main {
    static {
      System.loadLibrary("YourLibraryName");
      // The name  of the library is specified in Android.mk / build.gradle / Makefile / CMakeLists.txt, 
      // depending on your build system.
    }
}
```

If you package your native library in a jar, you can also use the [`NativeLibLoader`](https://github.com/cross-language-cpp/djinni-support-lib/blob/main/java/com/dropbox/djinni/NativeLibLoader.java)
to help unpack and load your lib(s).

Any library loaded from Java should provide the functions `JNI_OnLoad` and `JNI_OnUnload`.
They are called by JNI when the library is loaded/unloaded.

Djinni uses these functions to initialize & cleanup internal structures.
The generated file `djinni_jni_main.cpp` includes a default implementation of `JNI_Onload` and `JNI_OnUnload` functions 
provided by the support library.

If you are building a library that does not use JNI except through Djinni, this default should work well for you.
If want to provide your own implementation of `JNI_Onload` and `JNI_OnUnload`, the generation of `djinni_jni_main.cpp` can be disabled by setting `--jni-generate-main=false`.

## Objective-C / C++ Project

### Includes & Build Target
Generated files for Objective-C / C++ are as follows (assuming prefix is `DB`):

| Type       | C++ header                 | C++ source                   | Objective-C files         | Objective-C++ files         |
|------------|----------------------------|------------------------------|---------------------------|-----------------------------|
| Enum/Flags | my\_enum.hpp               |                              | DBMyEnum.h                |                             |
|            | my\_enum+json.hpp :three:  |                              |                           |                             |
| Record     | my\_record[\_base].hpp     | my\_record[\_base].cpp :one: | DBMyRecord[Base].h        | DBMyRecord[Base]+Private.h  |
|            | my\_record[\_base]+json.hpp|                              | DBMyRecord[Base].mm :two: | DBMyRecord[Base]+Private.mm |
|            |                     :three:|                              |                           |                             |
| Interface  | my\_interface.hpp          | my\_interface.cpp :one:      | DBMyInterface.h           | DBMyInterface+Private.h     |
|            |                            |                              |                           | DBMyInterface+Private.mm    |

- :one: Generated only for types that contain constants.
- :two: Generated only for types with derived operations and/or constants. These have `.mm` extensions to allow non-trivial constants.
- :three: Generated only if cpp json serialization is enabled.

Add all generated files to your build target, and link against the [djinni-support-lib](https://github.com/cross-language-cpp/djinni-support-lib).

Note that `+Private` files can only be used with ObjC++ source (other headers are pure ObjC) and are not required by Objective-C users of your interface.






## C++ JSON Serialization support

Serialization from C++ types to/from JSON is supported. This feature is currently only enabled for `nlohmann/json`, and if enabled creates `to_json`/`from_json` methods for all djinni records and enums.

```cpp
#include "my_record.hpp"
#include "my_record+json.hpp"
#include <iostream>
#include <nlohmann/json.hpp>

void foo(const my_record& record) {
  // convert record to json object
  nlohmann::json j = record;
  // dump serialized string
  std::cerr << j.dump(4);
  // create new instance of record from json object
  my_record cloned = j.get<my_record>();
}
```

### Json support for the date data type

Since there are many ways of converting a date to and from json, a simple implementation is provided by default which stores the date as the number of milliseconds elapsed since 00:00:00 UTC on January 1, 1970.

This default can be deactivated by adding a `-DDJINNI_CUSTOM_JSON_DATE` compilation flag to your compiler; in this case, you can implement your own date json serializer which better matches your requirements.

One such solution using [Howard Hinnant's date library](https://github.com/HowardHinnant/date) could be implemented as follows:

```cpp
#include <nlohmann/json.hpp>
#include <date/date.h>

namespace nlohmann {
    template <>
    struct adl_serializer<std::chrono::system_clock::time_point>
    {
        static void to_json(json &j, const std::chrono::system_clock::time_point& tp) {
            j = date::format("%F %T %Z", tp);
        }

        static void from_json(const json &j, std::chrono::system_clock::time_point& value) {
            if (j.is_null()) {
                auto dur = std::chrono::milliseconds(0);
                value = std::chrono::time_point<std::chrono::system_clock>(dur);
            } else {
                std::istringstream json_time{j.get<std::string>()};
                std::chrono::system_clock::time_point parsed_time{};
                // Time saved in UTC, so no need to extract time zone
                json_time >> date::parse("%F %T", value);
            }
        }
    };
}
```