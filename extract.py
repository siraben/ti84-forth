import argparse
from PIL import Image
import numpy as np

def subrect(bitmap, x, y, width, height, subsample):
    sh = height // subsample
    sw = width // subsample
    return bitmap[y:y + sh * subsample:subsample, x:x + sw * subsample:subsample]

def decode_bytes(bitmap):
    bitmap_flat = (bitmap.ravel() == 0).astype(np.uint8)
    return np.packbits(bitmap_flat)

def png_to_bytes(image_path):
    img = Image.open(image_path).convert('L')
    data = np.array(img)
    offset = 2
    bit_size = 4
    height, width = data.shape
    transformed_data = subrect(data, offset, offset, width - 2 * offset, height - 2 * offset, bit_size)
    return decode_bytes(transformed_data)

def main():
    parser = argparse.ArgumentParser(description='Extract and decode binary data from a PNG image.')
    parser.add_argument('image_path', type=str, help='Path to the PNG image file')
    parser.add_argument('-o', '--output', type=str, help='Path to save the decoded output')
    args = parser.parse_args()

    decoded_bytes = png_to_bytes(args.image_path)
    decoded_string = decoded_bytes.tobytes().decode(errors='ignore')
    
    if args.output:
        with open(args.output, 'w') as out_file:
            out_file.write(decoded_string)
    else:
        print(decoded_string)

if __name__ == '__main__':
    main()
