use interoptopus::indented;
use interoptopus::lang::c::{CType, CompositeType, Constant, Documentation, EnumType, Field, FnPointerType, Function, Meta, OpaqueType, Parameter, PrimitiveType, Variant};
use interoptopus::patterns::callbacks::NamedCallback;
use interoptopus::patterns::service::Service;
use interoptopus::patterns::{LibraryPattern, TypePattern};
use interoptopus::util::{longest_common_prefix, sort_types_by_dependencies};
use interoptopus::writer::{IndentWriter, WriteFor};
use interoptopus::{Error, Inventory};

use crate::config::{CDocumentationStyle, CFunctionStyle, CIndentationStyle, ToNamingStyle};
use crate::converter::{CTypeConverter, FunctionNameFlavor};
use crate::converter::Converter;
use crate::Config;

/// Writes the C file format, `impl` this trait to customize output.
pub trait CWriter {
    /// Returns the user config.
    fn config(&self) -> &Config;

    /// Returns the library to produce bindings for.
    fn inventory(&self) -> &Inventory;

    /// Returns the library to produce bindings for.
    fn converter(&self) -> &Converter;

    fn should_emit_by_meta(&self, meta: &Meta) -> bool {
        let rval = meta.namespace() == self.config().namespace_id;
        rval
    }

    fn write_patterns(&self, w: &mut IndentWriter) -> Result<(), Error> {
        for pattern in self.inventory().patterns() {
            match pattern {
                LibraryPattern::Service(cls) => {
                    if self.should_emit_by_meta(cls.the_type().meta()) {
                        self.write_pattern_service(w, cls)?
                    } else {
                        self.write_pattern_service(w, cls)?
                    }
                }
            }
        }

        Ok(())
    }

    fn write_pattern_service(&self, w: &mut IndentWriter, class: &Service) -> Result<(), Error> {
        let mut all_functions = class.constructors().to_vec();
        all_functions.extend_from_slice(class.methods());
        all_functions.push(class.destructor().clone());
    
        let context_type_name = class.the_type().rust_name();
        let common_prefix = longest_common_prefix(&all_functions);
    
        self.write_documentation(w, class.the_type().meta().documentation())?;
        
        // Ensure the class definition is within the C++ guard
        indented!(w, r#"#ifdef __cplusplus"#)?;
        indented!(w, r#"class {}"#, context_type_name)?;
        indented!(w, r#"{{"#)?;
        w.indent();
        indented!(w, r#"private:"#)?;
        w.indent();
        indented!(w, r#"{}* _context;"#, self.converter().opaque_to_typename(class.the_type()))?;
        w.unindent();
        indented!(w, r#"public:"#)?;
        w.indent();
        w.newline()?;
    
        indented!(w, r#"{}() : _context(nullptr) {{}}"#, context_type_name)?;
        indented!(w, r#"~{}() {{ Dispose(); }}"#, context_type_name)?;
        w.newline()?;
    
        for ctor in class.constructors() {
            let fn_name = self
                .converter()
                .function_name_to_c_name(ctor, FunctionNameFlavor::CPPMethodNameWithoutClass(&common_prefix));
            let rval = format!("static {}", context_type_name);
    
            self.write_documentation(w, ctor.meta().documentation())?;
            self.write_pattern_service_method(w, class, ctor, &rval, &fn_name, true, true, WriteFor::Code)?;
            w.newline()?;
        }
    
        self.write_pattern_service_method(w, class, class.destructor(), "void", "Dispose", true, false, WriteFor::Code)?;
        w.newline()?;
    
        for function in class.methods() {
            let fn_name = self
                .converter()
                .function_name_to_c_name(function, FunctionNameFlavor::CPPMethodNameWithoutClass(&common_prefix));
            let rval = match function.signature().rval() {
                CType::Pattern(TypePattern::FFIErrorEnum(_)) => "void".to_string(),
                CType::Pattern(TypePattern::AsciiPointer) => "const char*".to_string(),
                _ => self.converter().to_type_specifier(function.signature().rval()),
            };
            self.write_documentation(w, function.meta().documentation())?;
            self.write_pattern_service_method(w, class, function, &rval, &fn_name, false, false, WriteFor::Code)?;
            w.newline()?;
        }
    
        indented!(w, r#"{}* Context() const {{ return _context; }}"#, self.converter().opaque_to_typename(class.the_type()))?;
        w.unindent();
        indented!(w, r#"private:"#)?;
        w.indent();
        indented!(w, r#"explicit {}({}* context) : _context(context) {{}}"#, context_type_name, self.converter().opaque_to_typename(class.the_type()))?;
        w.unindent();
        indented!(w, r#"public:"#)?;
        w.indent();
        indented!(w, r#"static {} FromContext({}* context) {{ return {}(context); }}"#,  context_type_name, self.converter().opaque_to_typename(class.the_type()), context_type_name)?;
        w.unindent();
        w.unindent();
        indented!(w, r#"}};"#)?;
        indented!(w, r#"#endif /* __cplusplus */"#)?;
        w.newline()?;
    
        Ok(())
    }
    
    fn write_pattern_service_method(
        &self,
        w: &mut IndentWriter,
        class: &Service,
        function: &Function,
        rval: &str,
        fn_name: &str,
        write_context_by_ref: bool,
        is_ctor: bool,
        write_for: WriteFor
    ) -> Result<(), Error> {
        let mut names = Vec::new();
        let mut to_invoke = Vec::new();
        let mut types = Vec::new();
    
        for p in function.signature().params().iter().skip(1) {
            let name = p.name();
            let native = self.converter().to_type_specifier(p.the_type());
            to_invoke.push(name.to_string());
            names.push(name);
            types.push(native);
        }
    
        let method_to_invoke = self.converter().function_name_to_c_name(
            function,
            FunctionNameFlavor::RawFFIName
        );
        let method_return_type = self.converter().to_type_specifier(function.signature().rval());
        let extra_args = if to_invoke.is_empty() {
            "".to_string()
        } else {
            format!(", {}", to_invoke.join(", "))
        };
    
        let context = if write_context_by_ref { if is_ctor { "&self._context" } else { "&_context"} } else { "_context" };
        let arg_tokens = names.iter().zip(types.iter()).map(|(n, t)| format!("{} {}", t, n)).collect::<Vec<_>>();
        let fn_call = format!(r#"{}({}{})"#, method_to_invoke, context, extra_args);
    
        let signature = format!(r#"{} {}({})"#, rval, fn_name, arg_tokens.join(", "));
        if write_for == WriteFor::Docs {
            indented!(w, r#"{};"#, signature)?;
            return Ok(())
        }
    
        indented!(w, "{}", signature)?;
        indented!(w, r#"{{"#)?;
    
        if is_ctor {
            indented!(w, [_], r#"{} self;"#, class.the_type().rust_name())?;
        }
    
        match function.signature().rval() {
            CType::Pattern(TypePattern::FFIErrorEnum(e)) => {
        
                indented!(w, [_], r#"{} rval = {};"#, method_return_type, fn_call)?;
                indented!(w, [_], r#"if (rval != {})"#, self.converter().enum_variant_to_name(e.the_enum(), e.success_variant()))?;
                indented!(w, [_], r#"{{"#)?;
                indented!(w, [_ _], r#"throw rval;"#)?;
                indented!(w, [_], r#"}}"#)?;
            }
            CType::Pattern(TypePattern::AsciiPointer) => {
                indented!(w, [_], r#"{} s = {};"#, method_return_type, fn_call)?;
                indented!(w, [_], r#"return Marshal.PtrToStringAnsi(s);"#)?;
            }
            CType::Primitive(PrimitiveType::Void) => {
                indented!(w, [_], r#"{};"#, fn_call)?;
            }
            _ => {
                indented!(w, [_], r#"return {};"#, fn_call)?;
            }
        }
    
        if is_ctor {
            indented!(w, [_], r#"return self;"#)?;
        }
    
        indented!(w, r#"}}"#)?;
    
        Ok(())
    }

    fn write_custom_defines(&self, w: &mut IndentWriter) -> Result<(), Error> {
        indented!(w, "{}", &self.config().custom_defines)
    }

    fn write_file_header_comments(&self, w: &mut IndentWriter) -> Result<(), Error> {
        indented!(w, "{}", &self.config().file_header_comment)
    }

    fn write_imports(&self, w: &mut IndentWriter) -> Result<(), Error> {
        indented!(w, r#"#include <stdint.h>"#)?;
        indented!(w, r#"#include <stdbool.h>"#)?;

        // Write any user supplied includes into the file.
        for include in &self.config().additional_includes {
            indented!(w, "#include {}", include)?;
        }

        Ok(())
    }

    fn write_constants(&self, w: &mut IndentWriter) -> Result<(), Error> {
        for constant in self.inventory().constants() {
            self.write_constant(w, constant)?;
        }

        Ok(())
    }

    fn write_constant(&self, w: &mut IndentWriter, constant: &Constant) -> Result<(), Error> {
        let name = self.converter().const_name_to_name(constant);
        let the_type = match constant.the_type() {
            CType::Primitive(x) => self.converter().primitive_to_typename(&x),
            _ => return Err(Error::Null),
        };

        if self.config().documentation == CDocumentationStyle::Inline {
            self.write_documentation(w, constant.meta().documentation())?;
        }

        indented!(w, r#"const {} {} = {};"#, the_type, name, self.converter().constant_value_to_value(constant.value()))?;

        Ok(())
    }

    fn write_functions(&self, w: &mut IndentWriter) -> Result<(), Error> {
        for function in self.inventory().functions() {
            self.write_function(w, function)?;
        }

        Ok(())
    }

    fn write_function(&self, w: &mut IndentWriter, function: &Function) -> Result<(), Error> {
        if self.config().documentation == CDocumentationStyle::Inline {
            self.write_documentation(w, function.meta().documentation())?;
        }

        match self.config().function_style {
            CFunctionStyle::Typedefs => self.write_function_as_typedef_declaration(w, function)?,
            CFunctionStyle::ForwardDeclarations => self.write_function_declaration(w, function, 999)?,
        }

        if self.config().documentation == CDocumentationStyle::Inline {
            w.newline()?;
        }

        Ok(())
    }

    fn write_function_declaration(&self, w: &mut IndentWriter, function: &Function, max_line: usize) -> Result<(), Error> {
        let attr = &self.config().function_attribute;
        let rval = self.converter().to_type_specifier(function.signature().rval());
        let name = self.converter().function_name_to_c_name(function, FunctionNameFlavor::RawFFIName);

        let mut params = Vec::new();

        for (_, p) in function.signature().params().iter().enumerate() {
            match p.the_type() {
                CType::Array(a) => {
                    params.push(format!(
                        "{} {}[{}]",
                        self.converter().to_type_specifier(a.array_type()),
                        p.name().to_naming_style(&self.config().function_parameter_naming),
                        a.len(),
                    ));
                }
                _ => {
                    params.push(format!(
                        "{} {}",
                        self.converter().to_type_specifier(p.the_type()),
                        p.name().to_naming_style(&self.config().function_parameter_naming)
                    ));
                }
            }
        }

        // Test print line to see if we need to break it
        let line = format!(r#"{}{} {}({});"#, attr, rval, name, params.join(", "));

        if line.len() <= max_line {
            indented!(w, r#"{}{} {}({});"#, attr, rval, name, params.join(", "))?
        } else {
            indented!(w, r#"{}{} {}("#, attr, rval, name)?;
            for p in params {
                indented!(w, [_], r#"{}"#, p)?;
            }
            indented!(w, [_], r#");"#)?
        }

        Ok(())
    }

    fn write_function_as_typedef_declaration(&self, w: &mut IndentWriter, function: &Function) -> Result<(), Error> {
        let _attr = &self.config().function_attribute;
        let rval = self.converter().to_type_specifier(function.signature().rval());
        let name = self.converter().function_name_to_c_name(function, FunctionNameFlavor::RawFFIName);

        let mut params = Vec::new();

        for (_, p) in function.signature().params().iter().enumerate() {
            match p.the_type() {
                CType::Array(a) => {
                    params.push(format!("{} [{}]", self.converter().to_type_specifier(a.array_type()), a.len(),));
                }
                _ => {
                    params.push(format!("{}", self.converter().to_type_specifier(p.the_type()),));
                }
            }
        }

        indented!(w, r#"typedef {} (*{})({});"#, rval, name, params.join(", "))?;

        Ok(())
    }

    fn write_documentation(&self, w: &mut IndentWriter, documentation: &Documentation) -> Result<(), Error> {
        for line in documentation.lines() {
            indented!(w, r#"///{}"#, line)?;
        }

        Ok(())
    }

    fn write_type_definitions(&self, w: &mut IndentWriter) -> Result<(), Error> {
        let mut known_function_pointers = vec![];

        for the_type in &sort_types_by_dependencies(self.inventory().ctypes().to_vec()) {
            self.write_type_definition(w, the_type, &mut known_function_pointers)?;
        }

        Ok(())
    }

    fn write_type_definition(&self, w: &mut IndentWriter, the_type: &CType, known_function_pointers: &mut Vec<String>) -> Result<(), Error> {
        match the_type {
            CType::Primitive(_) => {}
            CType::Array(_) => {}
            CType::Enum(e) => {
                self.write_type_definition_enum(w, e)?;
                w.newline()?;
            }
            CType::Opaque(o) => {
                self.write_type_definition_opaque(w, o)?;
            }

            CType::Composite(c) => {
                self.write_type_definition_composite(w, c)?;
                w.newline()?;
            }
            CType::FnPointer(f) => {
                self.write_type_definition_fn_pointer(w, f, known_function_pointers)?;
                w.newline()?;
            }
            CType::ReadPointer(_) => {}
            CType::ReadWritePointer(_) => {}
            CType::Pattern(p) => match p {
                TypePattern::AsciiPointer => {}
                TypePattern::NamedCallback(e) => {
                    self.write_type_definition_named_callback(w, e)?;
                    w.newline()?;
                }
                TypePattern::FFIErrorEnum(e) => {
                    self.write_type_definition_enum(w, e.the_enum())?;
                    w.newline()?;
                }
                TypePattern::Slice(x) => {
                    self.write_type_definition_composite(w, x)?;
                    w.newline()?;
                }
                TypePattern::SliceMut(x) => {
                    self.write_type_definition_composite(w, x)?;
                    w.newline()?;
                }
                TypePattern::Option(x) => {
                    self.write_type_definition_composite(w, x)?;
                    w.newline()?;
                }
                TypePattern::Bool => {}
                TypePattern::CChar => {}
                TypePattern::APIVersion => {}
            },
        }
        Ok(())
    }

    fn write_type_definition_fn_pointer(&self, w: &mut IndentWriter, the_type: &FnPointerType, known_function_pointers: &mut Vec<String>) -> Result<(), Error> {
        self.write_type_definition_fn_pointer_body(w, the_type, known_function_pointers)
    }

    fn write_type_definition_fn_pointer_body(&self, w: &mut IndentWriter, the_type: &FnPointerType, known_function_pointers: &mut Vec<String>) -> Result<(), Error> {
        let rval = self.converter().to_type_specifier(the_type.signature().rval());
        let name = self.converter().fnpointer_to_typename(the_type);

        let mut params = Vec::new();
        for (i, param) in the_type.signature().params().iter().enumerate() {
            params.push(format!("{} x{}", self.converter().to_type_specifier(param.the_type()), i));
        }

        let fn_pointer = format!("typedef {} (*{})({});", rval, name, params.join(", "));

        if !known_function_pointers.contains(&fn_pointer) {
            indented!(w, "{}", fn_pointer)?;
            known_function_pointers.push(fn_pointer);
        }

        Ok(())
    }

    fn write_type_definition_named_callback(&self, w: &mut IndentWriter, the_type: &NamedCallback) -> Result<(), Error> {
        self.write_type_definition_named_callback_body(w, the_type)
    }

    fn write_type_definition_named_callback_body(&self, w: &mut IndentWriter, the_type: &NamedCallback) -> Result<(), Error> {
        let rval = self.converter().to_type_specifier(the_type.fnpointer().signature().rval());
        let name = self.converter().named_callback_to_typename(the_type);

        let mut params = Vec::new();
        for param in the_type.fnpointer().signature().params().iter() {
            params.push(format!(
                "{} {}",
                self.converter().to_type_specifier(param.the_type()),
                param.name().to_naming_style(&self.config().function_parameter_naming)
            ));
        }

        indented!(w, "{}", format!("typedef {} (*{})({});", rval, name, params.join(", ")))?;

        Ok(())
    }

    fn write_type_definition_enum(&self, w: &mut IndentWriter, the_type: &EnumType) -> Result<(), Error> {
        let name = self.converter().enum_to_typename(the_type);

        if self.config().documentation == CDocumentationStyle::Inline {
            self.write_documentation(w, the_type.meta().documentation())?;
        }

        self.write_braced_declaration_opening(w, format!("typedef enum {}", name))?;

        for variant in the_type.variants() {
            self.write_type_definition_enum_variant(w, variant, the_type)?;
        }

        self.write_braced_declaration_closing(w, name)
    }

    fn write_type_definition_enum_variant(&self, w: &mut IndentWriter, variant: &Variant, the_enum: &EnumType) -> Result<(), Error> {
        let variant_name = self.converter().enum_variant_to_name(the_enum, variant);
        let variant_value = variant.value();

        if self.config().documentation == CDocumentationStyle::Inline {
            self.write_documentation(w, variant.documentation())?
        }

        indented!(w, r#"{} = {},"#, variant_name, variant_value)
    }

    fn write_type_definition_opaque(&self, w: &mut IndentWriter, the_type: &OpaqueType) -> Result<(), Error> {
        if self.config().documentation == CDocumentationStyle::Inline {
            self.write_documentation(w, the_type.meta().documentation())?;
        }

        self.write_type_definition_opaque_body(w, the_type)?;

        if self.config().documentation == CDocumentationStyle::Inline {
            w.newline()?;
        }

        Ok(())
    }

    fn write_type_definition_opaque_body(&self, w: &mut IndentWriter, the_type: &OpaqueType) -> Result<(), Error> {
        let name = self.converter().opaque_to_typename(the_type);
        indented!(w, r#"typedef struct {} {};"#, name, name)
    }

    fn write_type_definition_composite(&self, w: &mut IndentWriter, the_type: &CompositeType) -> Result<(), Error> {
        if self.config().documentation == CDocumentationStyle::Inline {
            self.write_documentation(w, the_type.meta().documentation())?;
        }

        let name = self.converter().composite_to_typename(the_type);

        if the_type.is_empty() {
            // C doesn't allow us writing empty structs.
            indented!(w, r#"typedef struct {} {};"#, name, name)?;
            Ok(())
        } else {
            self.write_type_definition_composite_body(w, the_type)
        }
    }

    fn write_type_definition_composite_body(&self, w: &mut IndentWriter, the_type: &CompositeType) -> Result<(), Error> {
        let name = self.converter().composite_to_typename(the_type);

        let alignment = the_type.meta().alignment();
        if let Some(align) = alignment {
            indented!(w, "#pragma pack(push, {})", align)?;
        }

        self.write_braced_declaration_opening(w, format!(r#"typedef struct {}"#, name))?;

        for field in the_type.fields() {
            self.write_type_definition_composite_body_field(w, field, the_type)?;
        }

        self.write_braced_declaration_closing(w, name)?;

        if alignment.is_some() {
            indented!(w, "#pragma pack(pop)")?;
        }
        Ok(())
    }

    fn write_type_definition_composite_body_field(&self, w: &mut IndentWriter, field: &Field, _the_type: &CompositeType) -> Result<(), Error> {
        if self.config().documentation == CDocumentationStyle::Inline {
            self.write_documentation(w, field.documentation())?;
        }

        match field.the_type() {
            CType::Array(x) => {
                let field_name = field.name();
                let type_name = self.converter().to_type_specifier(x.array_type());
                indented!(w, r#"{} {}[{}];"#, type_name, field_name, x.len())
            }
            _ => {
                let field_name = field.name();
                let type_name = self.converter().to_type_specifier(field.the_type());
                indented!(w, r#"{} {};"#, type_name, field_name)
            }
        }
    }

    fn write_ifndef(&self, w: &mut IndentWriter, f: impl FnOnce(&mut IndentWriter) -> Result<(), Error>) -> Result<(), Error> {
        if self.config().directives {
            indented!(w, r#"#ifndef {}"#, self.config().ifndef)?;
            indented!(w, r#"#define {}"#, self.config().ifndef)?;
            w.newline()?;
        }

        f(w)?;

        if self.config().directives {
            w.newline()?;
            indented!(w, r#"#endif /* {} */"#, self.config().ifndef)?;
        }

        Ok(())
    }

    fn write_ifdefcpp(&self, w: &mut IndentWriter, f: impl FnOnce(&mut IndentWriter) -> Result<(), Error>) -> Result<(), Error> {
        if self.config().directives {
            indented!(w, r#"#ifdef __cplusplus"#)?;
            indented!(w, r#"extern "C" {{"#)?;
            indented!(w, r#"#endif"#)?;
            w.newline()?;
        }

        f(w)?;

        if self.config().directives {
            w.newline()?;
            indented!(w, r#"#ifdef __cplusplus"#)?;
            indented!(w, r#"}}"#)?;
            indented!(w, r#"#endif"#)?;
        }
        Ok(())
    }

    fn write_all(&self, w: &mut IndentWriter) -> Result<(), Error> {
        self.write_file_header_comments(w)?;
        w.newline()?;

        self.write_ifndef(w, |w| {
            self.write_ifdefcpp(w, |w| {
                if self.config().imports {
                    self.write_imports(w)?;
                    w.newline()?;
                }
                
                self.write_custom_defines(w)?;
                w.newline()?;
                
                self.write_constants(w)?;
                w.newline()?;
                
                self.write_type_definitions(w)?;
                w.newline()?;
                
                self.write_functions(w)?;
                
                if self.config().cpp_compatibility {
                    self.write_patterns(w)?;
                }
                
                Ok(())
            })?;

            Ok(())
        })?;

        Ok(())
    }

    fn write_braced_declaration_opening(&self, w: &mut IndentWriter, definition: String) -> Result<(), Error> {
        match self.config().indentation {
            CIndentationStyle::Allman => {
                indented!(w, "{}", definition)?;
                indented!(w, "{{")?;
                w.indent();
            }
            CIndentationStyle::KAndR => {
                indented!(w, "{} {{", definition)?;
                w.indent();
            }
            CIndentationStyle::GNU => {
                indented!(w, "{}", definition)?;
                indented!(w, "  {{")?;
                w.indent();
            }
            CIndentationStyle::Whitesmiths => {
                indented!(w, "{}", definition)?;
                indented!(w, [_], "{{")?;
                w.indent();
            }
        }

        Ok(())
    }

    fn write_braced_declaration_closing(&self, w: &mut IndentWriter, name: String) -> Result<(), Error> {
        match self.config().indentation {
            CIndentationStyle::Allman | CIndentationStyle::KAndR => {
                w.unindent();
                indented!(w, "}} {};", name)?;
            }
            CIndentationStyle::GNU => {
                w.unindent();
                indented!(w, "  }} {};", name)?;
            }
            CIndentationStyle::Whitesmiths => {
                w.unindent();
                indented!(w, [_], "}} {};", name)?;
            }
        }

        Ok(())
    }
}

#[doc(hidden)]
pub struct Helper<'a> {
    pub config: &'a Config,
    pub converter: &'a dyn CTypeConverter,
}

/// Writes common service overload code
fn write_common_service_method_overload<FPatternMap: Fn(&Helper, &Parameter) -> String>(
    w: &mut IndentWriter,
    h: Helper,
    function: &Function,
    fn_pretty: &str,
    f_pattern: FPatternMap,
    write_for: WriteFor,
) -> Result<(), Error> {
    let mut names = Vec::new();
    let mut to_invoke = Vec::new();
    let mut types = Vec::new();

    // Write checked method. These are "normal" methods that accept
    // common C# types.
    let rval = match function.signature().rval() {
        CType::Pattern(TypePattern::FFIErrorEnum(_)) => "void".to_string(),
        CType::Pattern(TypePattern::AsciiPointer) => "string".to_string(),
        _ => h.converter.to_type_specifier(function.signature().rval()),
    };

    // For every parameter except the first, figure out how we should forward
    // it to the invocation we perform.
    for p in function.signature().params().iter().skip(1) {
        let name = p.name();

        // If we call the checked function we want to resolve a `SliceU8` to a `byte[]`,
        // but if we call the unchecked version we want to keep that `Sliceu8` in our signature.
        // let native = self.to_typespecifier_in_param(p.the_type());
        let native = f_pattern(&h, p);

        // Forward `ref` and `out` accordingly.
        if native.contains("out ") {
            to_invoke.push(format!("out {}", name));
        } else if native.contains("ref ") {
            to_invoke.push(format!("ref {}", name));
        } else {
            to_invoke.push(name.to_string());
        }

        names.push(name);
        types.push(native);
    }

    let method_to_invoke = h.converter.function_name_to_c_name(
        function,
        match h.config.rename_symbols {
            true => FunctionNameFlavor::CPPMethodNameWithClass,
            false => FunctionNameFlavor::RawFFIName,
        },
    );
    let extra_args = if to_invoke.is_empty() {
        "".to_string()
    } else {
        format!(", {}", to_invoke.join(", "))
    };

    // Assemble actual function call.
    let context = "_context";
    let arg_tokens = names.iter().zip(types.iter()).map(|(n, t)| format!("{} {}", t, n)).collect::<Vec<_>>();
    let fn_call = format!(r#"{}.{}({}{})"#, h.config.class, method_to_invoke, context, extra_args);

    let signature = format!(r#"public {} {}({})"#, rval, fn_pretty, arg_tokens.join(", "));
    if write_for == WriteFor::Docs {
        indented!(w, "{};", signature)?;
        return Ok(());
    }

    // Write signature.
    indented!(w, "{}", signature)?;
    indented!(w, r#"{{"#)?;

    match function.signature().rval() {
        CType::Pattern(TypePattern::FFIErrorEnum(_)) => {
            indented!(w, [_], r#"{};"#, fn_call)?;
        }
        CType::Primitive(PrimitiveType::Void) => {
            indented!(w, [_], r#"{};"#, fn_call)?;
        }
        _ => {
            indented!(w, [_], r#"return {};"#, fn_call)?;
        }
    }

    indented!(w, r#"}}"#)?;

    Ok(())
}