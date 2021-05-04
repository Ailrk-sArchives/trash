// a value with alignment n must be stored at
// an address that is multiple of n.

// by default, composite structures have an alignment equal to
// the max of their fields alignment.

/// repr(Rust)
/// Composite types will have all fields aligned by the largest field.
// This struct will be 32 bit aligned.
struct A {
    a: u8,
    b: u32,
    c: u16,
}

// and it might look like this in memory
struct AAlignedLayout1 {
    a: u8,
    _pad1: [u8; 3], // align 32 bits.
    b: u32,
    c: u16,
    _pad2: [u8; 2], // align 32 bits.
}

// another posibility:
struct AAlignedLayout2 {
    a: u8,
    b: u32,
    c: u16,
    _pad: u8, // align 32 bits.
}

/// field ordering is not necessarily the same as their order in code.

// this is because certain order of layout is more space efficient.

struct Foo<T, U> {
    count: u16,
    data1: T,
    data2: U,
}

type Foo1 = Foo<u16, u32>;

// Foo1's layout will look like this, you can't be more efficient than this layout
// unless you do some hecky stuffs.
struct Foo1Layout {
    count: u16,
    data1: u16,
    data2: u32,
}

type Foo2 = Foo<u32, u16>;

// if you keep the order of fields in code, Foo2 will be quite wasteful.
struct Foo2Layout {
    count: u16,
    _pad1: u16, // wasteful
    data1: u32,
    _pad2: u16, // wasteful
    data2: u16,
}

/// enum is even more complicated

enum Bar {
    A(u32),
    B(u64),
    C(u8),
}

// this might looks like this
struct BarRepr {
    data: u64, // either u64, u32, or u8 based on tag.
    tag: u8,   // A = 0, B = 1, C = 2
}

/// Exotically Sized type
/// Some types might not have a statically known type at compile time.

/// Dynamicaly Sized types (DST)

struct MySuperSliceable<T: ?Sized> {
    info: u32,
    data: T,
}

#[cfg(test)]
mod test_dst {
    use super::MySuperSliceable;

    #[test]
    fn test_dst() {
        let sized: MySuperSliceable<[u8; 8]> = MySuperSliceable {
            info: 17,
            data: [0; 8],
        };

        // here, dynamic has dynamic size
        let dynamic: &MySuperSliceable<[u8]> = &sized;
        println!("{} {:?}", dynamic.info, &dynamic.data);
    }
}

/// zero sized type ZTS
// zero size
struct Nothing;

// the whole struct has size 0.
struct LostsOfNothing {
    foo: Nothing,
    qux: (),
    baz: [u8; 0],
}

/// Void type

// the opposite of unit. This type can only be talked about at the type level,
// but never the term level.
enum Void {}

#[cfg(test)]
mod test_void {
    use super::Void;

    #[test]
    fn test_void() {
        // saying error is impossible for this type.
        let res: Result<u32, Void> = Ok(0);
        let Ok(num) = res;
    }
}

/// repr(C)
/// using C representation to interface with C code.

#[repr(C)]
struct B {
    a: u8,
    b: u32,
    c: u16,
}

// force to align 1 byte.
#[repr(packed)]
struct C {
    a: u8,
    b: u32,
    c: u16,
}
