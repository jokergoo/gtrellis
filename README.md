Make Genome Level Trellis Graph
===============================

**gtrellis** aims to arrange chromosomes as Trellis style and support multiple tracks for visualization.

Basic usage is:

```r
library(gtrellis)
library(circlize)

bed = generateRandomBed()
gtrellis_layout(track_ylim = range(bed[[4]]))
add_track(bed, panel.fun = function(bed) {
    x = (bed[[2]] + bed[[3]]) / 2
    y = bed[[4]]
    grid.points(x, y, pch = 16, size = unit(0.5, "mm"))
})
```

Example figures are:

![1](https://cloud.githubusercontent.com/assets/449218/5553808/5d5859bc-8c30-11e4-9a58-77e28cf7fd8b.png)
![default](https://cloud.githubusercontent.com/assets/449218/6318159/bca7edf4-ba81-11e4-9cf7-e6bab5303ca0.png)
![1](https://cloud.githubusercontent.com/assets/449218/6318160/c061efc6-ba81-11e4-9b80-ab6ddb7377dc.png)
![4](https://cloud.githubusercontent.com/assets/449218/5553818/89527e8a-8c30-11e4-84fe-2d8b72ddcc2e.png)
![5](https://cloud.githubusercontent.com/assets/449218/5553819/911f938c-8c30-11e4-8b49-d2b32291aef5.png)
![6](https://cloud.githubusercontent.com/assets/449218/5553820/978ab882-8c30-11e4-8259-2b8c91e780ce.png)
