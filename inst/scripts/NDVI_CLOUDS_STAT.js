//VERSION=3
//ndvi_value and cloud_cover statistics

function setup() {
  return {
    input: [{
      bands: [
        "B04",
        "B08",
        "CLD",
        "dataMask"
      ]
    }],
    output: [
      {
        id: "statistics",
        bands: ["ndvi_value", "cloud_cover"],
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
        statistics: [ndvi, samples.CLD],
        dataMask: [samples.dataMask]
        }
}
