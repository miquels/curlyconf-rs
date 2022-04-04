use std::collections::HashMap;

pub trait CurlyConf {
    fn describe() -> Type;
}

#[derive(Debug)]
pub enum Type {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Char,
    String,
    Option(Box<Type>),
    Struct {
        key_field: Option<String>,
        fields: HashMap<String, Type>,
    },
    Map {
        key_field: Option<String>,
        val_type: Box<Type>,
    },
    List {
        key_field: Option<String>,
        elem_type: Box<Type>,
    }
}

macro_rules! curlyconf {
    ( $($rtype:ty => $dtype:ident),*) => {
        $(
            impl CurlyConf for $rtype {
                fn describe() -> Type {
                    Type::$dtype
                }
            }
        )*
    };
}
curlyconf!(bool => Bool);
curlyconf!(i8 => I8, i16 => I16, i32 => I32, i64 => I64, i128 => I128);
curlyconf!(u8 => U8, u16 => U16, u32 => U32, u64 => U64, u128 => U128);
curlyconf!(f32 => F32, f64 => F64, char => Char, String => String);

impl<T: CurlyConf> CurlyConf for Option<T> {
    fn describe() -> Type {
        Type::Option(Box::new(T::describe()))
    }
}

impl<T: CurlyConf> CurlyConf for Vec<T>
{
    fn describe() -> Type {
        Type::List {
            key_field: None,
            elem_type: Box::new(T::describe()),
        }
    }
}
