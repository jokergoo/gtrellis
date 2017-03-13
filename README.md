[![Build Status](https://travis-ci.org/jokergoo/gtrellis.svg)](https://travis-ci.org/jokergoo/gtrellis)  
[![codecov](https://img.shields.io/codecov/c/github/jokergoo/gtrellis.svg)](https://codecov.io/github/jokergoo/gtrellis) 
[![bioc](http://www.bioconductor.org/shields/downloads/gtrellis.svg)](https://bioconductor.org/packages/stats/bioc/gtrellis/) 
[![bioc](http://mcube.nju.edu.cn/cgi-bin/zuguanggu/bioc_download.pl?package=gtrellis)](https://bioconductor.org/packages/stats/bioc/gtrellis/) 
[![bioc](http://www.bioconductor.org/shields/years-in-bioc/gtrellis.svg)](http://bioconductor.org/packages/devel/bioc/html/gtrellis.html)


## Make Genome Level Trellis Graph

Genome level Trellis graph visualizes genomic data conditioned by genomic categories (e.g. chromosomes). For each genomic category, multiple dimensional data which are represented as tracks describe different features from different aspects. This package provides high flexibility to arrange genomic categories and to add self-defined graphics in the plot.

### Citation

Zuguang Gu, Roland Eils and Matthias Schlesner. [gtrellis: an R/Bioconductor package for making genome-level Trellis graphics](http://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-016-1051-4), 2016. BMC Bioinformatics.

### Install

**gtrellis** is available on [Bioconductor](http://bioconductor.org/packages/release/bioc/html/gtrellis.html), you can install it by:

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("gtrellis") 
```

If you want the latest version, install it directly from GitHub:

```{r}
library(devtools)
install_github("jokergoo/gtrellis")
```

### Usage

Basic usage is:

```r
library(gtrellis)

bed = circlize::generateRandomBed()
gtrellis_layout(track_ylim = range(bed[[4]]))
add_track(bed, panel.fun = function(bed) {
    x = (bed[[2]] + bed[[3]]) / 2
    y = bed[[4]]
    grid.points(x, y, pch = 16, size = unit(1, "mm"))
})
```

There are several tracks with pre-defined graphics:

- `add_points_track()`
- `add_lines_track()`
- `add_rect_track()`
- `add_segments_track()`
- `add_heatmap_track()`

### Layout

**gtrellis** supports several types of layouts:

One row layout:

![image](https://cloud.githubusercontent.com/assets/449218/13721936/9a511d74-e835-11e5-83f3-20952687ec7f.png)

Multiple row layout that chromosomes with similar length are arranged in a same column:

![image](https://cloud.githubusercontent.com/assets/449218/13721942/b7ac4e0c-e835-11e5-8937-6c2d87868f65.png)

Chromosomes in a same row are arranged compactly to the left:

![image](https://cloud.githubusercontent.com/assets/449218/13721947/c6a2d55c-e835-11e5-80df-645fae07ff2d.png)

For each chromosome, multiple tracks are supported:

![image](https://cloud.githubusercontent.com/assets/449218/13721950/d6e5eff8-e835-11e5-9932-e868f084a717.png)

### Example

Example figures are:

![image](https://cloud.githubusercontent.com/assets/449218/13721966/44ddc940-e836-11e5-8ac0-55e4751a606a.png)

![1](https://cloud.githubusercontent.com/assets/449218/6318160/c061efc6-ba81-11e4-9b80-ab6ddb7377dc.png)

![4](https://cloud.githubusercontent.com/assets/449218/5553818/89527e8a-8c30-11e4-84fe-2d8b72ddcc2e.png)

![5](https://cloud.githubusercontent.com/assets/449218/5553819/911f938c-8c30-11e4-8b49-d2b32291aef5.png)

![6](https://cloud.githubusercontent.com/assets/449218/5553820/978ab882-8c30-11e4-8259-2b8c91e780ce.png)

![image](https://cloud.githubusercontent.com/assets/449218/21886589/c6852188-d8bc-11e6-9d1d-8ef16eb30b0e.png)

![image](https://cloud.githubusercontent.com/assets/449218/21886590/cd5f0758-d8bc-11e6-8701-0b357c8bc8fa.png)

### License

GPL (>= 2)
