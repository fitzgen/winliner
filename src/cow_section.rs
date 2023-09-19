use wasm_encoder::SectionId;

pub enum CowSection<'a> {
    Borrowed(wasm_encoder::RawSection<'a>),
    Owned(OwnedSection),
}

impl<'a> wasm_encoder::Encode for CowSection<'a> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            CowSection::Borrowed(b) => b.encode(sink),
            CowSection::Owned(o) => o.encode(sink),
        }
    }
}

impl<'a> wasm_encoder::Section for CowSection<'a> {
    fn id(&self) -> u8 {
        match self {
            CowSection::Borrowed(b) => b.id(),
            CowSection::Owned(o) => o.id(),
        }
    }
}

pub struct OwnedSection {
    id: u8,
    data: Vec<u8>,
}

impl wasm_encoder::Encode for OwnedSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend(&self.data);
    }
}

impl wasm_encoder::Section for OwnedSection {
    fn id(&self) -> u8 {
        self.id
    }
}

pub fn borrowed<'a, T>(
    new_sections: &mut Vec<CowSection<'a>>,
    full_wasm: &'a [u8],
    reader: wasmparser::SectionLimited<T>,
    id: SectionId,
) {
    let id = id as u8;
    log::trace!("Borrowing section {id} and leaving it unmodified");
    new_sections.push(CowSection::Borrowed(wasm_encoder::RawSection {
        id,
        data: &full_wasm[reader.range()],
    }));
}

pub fn owned<'a>(new_sections: &mut Vec<CowSection<'a>>, section: impl wasm_encoder::Section) {
    let id = section.id();
    log::trace!("Adding instrumented section {id}");
    let mut data = vec![];
    section.encode(&mut data);
    new_sections.push(CowSection::Owned(OwnedSection { id, data }));
}
