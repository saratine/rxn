mod value;

pub struct Runtime {}

#[repr(u8)]
pub enum OpCode {
    Return,

}

pub struct Chunk {
    code: Vec<u8>,
}

impl Chunk {
    pub fn size(&self) -> usize {
        self.code.len()
    }

    pub fn disassemble(&self) {
        let mut offset = 0;
        let chunk_size = self.size();
        while offset < chunk_size {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        let op_code = unsafe { std::mem::transmute::<u8, OpCode>(self.code[offset]) };
        match op_code {
            OpCode::Return => offset + 1,
        }
    }
}
