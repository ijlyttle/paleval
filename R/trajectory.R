# License: BSD_3_clause
#
# YEAR: 2005-2018
#
# COPYRIGHT HOLDER: Ross Ihaka, Paul Murrell, Kurt Hornik, Jason C. Fisher, Reto
# Stauffer, Claus O. Wilke, Claire D. McWhite, Achim Zeileis
#
# ORGANIZATION: Ross Ihaka, Paul Murrell, Kurt Hornik, Jason C. Fisher, Reto
# Stauffer, Claus O. Wilke, Claire D. McWhite, Achim Zeileis
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its contributors
# may be used to endorse or promote products derived from this software without
# specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# https://github.com/cran/colorspace/blob/0d206bc2db4bd143710ed28490c4f8b1b05e8ea3/R/hcl_palettes.R#L397

## trajectories
lintrj <- function(i, p1, p2) p2 - (p2 - p1) * i
tritrj <- function(i, j, p1, p2, pm) ifelse(i <= j,
                                            p2 - (p2 - pm) * i/j,
                                            pm - (pm - p1) * abs((i - j)/(1 - j)))

## HCL sequence
seqhcl <- function(i, h1, h2, c1, c2, l1, l2, p1, p2, cmax, fixup, ...) {
  j <- 1/(1 + abs(cmax - c1) / abs(cmax - c2))
  if (!is.na(j) && (j <= 0 | j >= 1)) j <- NA
  colorspace::hex(colorspace::polarLUV(
    L = lintrj(i^p2, l1, l2),
    C = if(is.na(j)) lintrj(i^p1, c1, c2) else tritrj(i^p1, j, c1, c2, cmax),
    H = lintrj(i, h1, h2)),
    fixup = fixup, ...
  )
}
