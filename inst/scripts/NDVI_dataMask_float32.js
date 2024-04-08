//VERSION=3
//NDVI as 32-bit floating point number

function setup() {
  return {
    input: [{
      bands: [
        "B04",
        "B08",
        "dataMask"
      ]
    }],
    output: [
      {
        id: "statistics",
        bands: ["ndvi_value"],
        sampleType: "FLOAT32"
      },
      {
        id: "dataMask",
        bands: 1
      }]
  }
}

function evaluatePixel(samples) {
    let ndvi = index(samples.B08, samples.B04)
    if (ndvi < 0) {
        ndvi = 0
    }
    return {
        statistics: [ndvi],
        dataMask: [samples.dataMask]
        }
}


