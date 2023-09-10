//VERSION=3
//NDVI as unsigned 8-bit integer (byte) [range: 0-255]

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B04", "B08"]
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "UINT8"
        }
    };
}

function evaluatePixel(sample) {
    // this computes the NDVI value
    return [Math.round(index(sample.B08, sample.B04) * 255)];
}

