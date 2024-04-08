//VERSION=3
//NDVI as 32-bit floating point number

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B04", "B08"]
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "FLOAT32"
        }
    };
}

function evaluatePixel(sample) {
    // this computes the NDVI value
    return [index(sample.B08, sample.B04)];
}

