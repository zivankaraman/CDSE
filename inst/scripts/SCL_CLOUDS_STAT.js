//VERSION=3
//cloud_cover statistics

function setup() {
  return {
    input: [{
      bands: [
        "SCL",
        "dataMask"
      ]
    }],
    output: [
      {
        id: "statistics",
        bands: ["cloud_cover"],
        sampleType: "FLOAT32"
      },
      {
        id: "dataMask",
        bands: 1
      }]
  }
}

function evaluatePixel(samples) {
    var clouds;
    clouds = 0;
    switch (samples.SCL) {
      // No Data (Missing data)
      case 0: clouds = 1;
      // Saturated or defective pixel
      case 1: clouds = 1;
      // Cloud shadows
      case 3: clouds = 1;
      // Cloud medium probability
      case 8: clouds = 1;
      // Cloud high probability
      case 9: clouds = 1;
      // Thin cirrus
      case 10: clouds = 1;
    }

    return {
        statistics: [clouds],
        dataMask: [samples.dataMask]
        }
}


