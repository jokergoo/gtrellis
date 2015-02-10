\name{get_current_cell_meta_data}
\alias{get_current_cell_meta_data}
\title{
get current cell meta data  


}
\description{
get current cell meta data  


}
\usage{
get_current_cell_meta_data(name)
}
\arguments{

  \item{name}{name}

}
\details{
Following meta data can be retrieved:  

\describe{
  \item{name}{name}
  \item{xlim}{xlim without including padding, cells in a same column shares the same \code{xlim}}
  \item{ylim}{yiim without including padding}
  \item{extended_xlim}{xlim with padding}
  \item{extended_ylim}{ylim with padding}
  \item{original_xlim}{xlim in original data}
  \item{original_ylim}{ylim in original data}
  \item{i_col}{which column in the layout}
  \item{i_row}{which row in the layout}
  \item{i_track}{which track in the layout}
}


}
