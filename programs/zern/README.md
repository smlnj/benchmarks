# The zern Benchmark

This program was taken from the MLton benchmarks and split into separate files.  It
was originally written by David McClain as the subject of a language study.
He describes the program as follows:

> As a directly measurable test of the conclusions of the study, I chose to develop
> a correct BlurFit routine. This routine is enormously difficult to program correctly.
> It utilizes the Levenberg-Marquardt algorithm to arrange a linear combination of
> Zernike polynomials over a 2-D annular region, representing wavefront phase
> distortions in an exit pupil, such that the imaged PSF has the same blur in the
> focal plane as was measured in the laboratory. The mathematics of this process is
> substantial and difficult. Each iteration requires 3N FFT's, where N is the number
> of Zernike polynomials under consideration.

An archived version of the article is at

	https://web.archive.org/web/20020207103454/http://www.azstarnet.com/~dmcclain/LanguageStudy.html

