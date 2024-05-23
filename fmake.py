import argparse
import subprocess
import sys
import os

def hexdump_to_asm(file_path):
    output_asm = f"{os.path.splitext(file_path)[0]}.asm"
    with open(file_path, 'rb') as f:
        hex_data = f.read().hex()
    
    formatted_hex = ""
    for i in range(0, len(hex_data), 32):
        line = hex_data[i:i+32]
        formatted_line = ", ".join([f"$" + line[j:j+2] for j in range(0, len(line), 2)])
        formatted_hex += f".db {formatted_line}\n"
    
    # Add a null byte at the end
    formatted_hex += ".db $00\n"

    with open(output_asm, 'w') as f:
        f.write(formatted_hex)
    
    return output_asm

def assemble_to_8xp(asm_path):
    output_8xp = f"{os.path.splitext(asm_path)[0]}.8xp"
    spasm_command = f"./spasm {asm_path} {output_8xp}"
    subprocess.run(spasm_command, shell=True)

def fmake(file_path, assemble):
    asm_path = hexdump_to_asm(file_path)
    if assemble:
        assemble_to_8xp(asm_path)
        os.remove(asm_path)
    else:
        print(f"Assembly file created: {asm_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Convert Forth source files to TI-84+ executable format.')
    parser.add_argument('file_path', type=str, help='Path to the Forth source file.')
    parser.add_argument('--assemble', action='store_true', help='Assemble the .asm file to .8xp executable.')

    args = parser.parse_args()
    fmake(args.file_path, args.assemble)
