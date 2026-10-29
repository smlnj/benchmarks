
from math import fabs, sin, cos
from .sdsmisc import  FindRange, getinc, _catinfo, _numinfo, catspace, _write_svgfile, \
           numspace, axispos, stubpos, polar, arrowhead, htmlcode, dpcluster, nicelog
from .sdsdt   import  _intdt,  _outdt,  _dtdiff,  _finalize_dt_range,  _datestubs


def write_svgfile(svgcode, filename):   # wrapper 
    _write_svgfile(svgcode, filename)
    return


class DataDraw:

    def __init__(self, logmsg=False):
        self.svg    = {'active':False}    # result svg code and parms
        self.space  = []     # xspace, yspace
        self.ln     = {}     # line parms
        self.myline = None   # user's default line parms
        self.text   = {}     # text parms
        self.mytext = None   # user's default text parms
        self.fr     = FindRange()
        self.curve  = {}     # lineplot curve accumulator
        self.leg    = []     # legend
        self.gstate = 0      # gtag state,  0= inactive, 1= active, 2= active with <a>
        self.dtformat = '%Y-%m-%d'
        self.weekday0 = 0
        self.fymonth1 = 1
        self.iax    = {'X':0, 'Y':1, 'x':0, 'y':1} 
        self.dpclustering = None
        self.crossorigin = None
        if logmsg:
            print('datadraw initialized')
        return


    def svgbegin(self, width=None, height=None, bgcolor=None, svgtag=None, 
                  outline=False, rounded=None, testpat=None):
        """ reset and begin a new svg graphic """
        self.space  = [{'scaletype':None}, {'scaletype':None}] 
        self.gstate = 0   
        self.settext(reset=True)
        self.setline(reset=True)
        self.dpclustering = None
        self.curve  = {'natx':None, 'naty':None}
        self.leg    = []
        self.dtformat = '%Y-%m-%d'
        self.weekday0 = 0
        self.fymonth1 = 1
        self.crossorigin = None
        self.svg = {'active':True, 'out':'', 'height':0, 'width':0}  # early init
        try: 
            width = int(width); 
            height = int(height)
        except: 
            raise ValueError('svgbegin: width, height must be specified as integer')

        self.svg = {'active':True, 'out':'', 'width':width, 'height':height}
        if svgtag:
            self.svg['out'] = customtag + '\n'   # user-supplied custom <svg> tag
        else:
            self.svg['out'] = str('<svg xmlns=\"http://www.w3.org/2000/svg\" sds="23" '
                                 f'width="{width}" height="{height}">\n' )
        if bgcolor or outline:  
            bgcolor = '#fff' if not bgcolor else bgcolor
            self.rect(0, 0, width, height, color=bgcolor, outline=outline, rounded=rounded)
        if testpat:
            self.test_pattern(width, height)
        return


    def svgresult(self, tofile=None, noclose=False):
        """ return the current svg chunk to caller, or write it to a file """
        if not self.svg['active']:
            raise RuntimeError('svgresult: no svg graphic begun yet')
        if noclose:
            result = self.svg['out']
            self.svg['out'] = ''   # to be continued subsequently
        else:
            result = self.svg['out'] + '\n</svg>\n'
            self.svg['active'] = False
        if tofile:
            _write_svgfile(result, tofile)
        return result


    def svgappend(self, svgcode):
        """ append arbitrary svg code to result buffer """
        self.svg['out'] += '\n'+svgcode+'\n'
        return


    def setline(self, reset=False, width=None, color=None, opacity='ns',
                   dash='ns', css='ns', style='ns', save=False, restore=False):
        """ set line attributes for subsequent drawing.  They stay in effect until changed.
            'ns' indicates 'not specified' where None is a valid specification.
            reset=True  sets all line parms back to 'factory' default state.
            restore=True  sets all parms to user defaults if any (otherwise 'factory')
            save=True    saves all current lineparms as user defaults
            With reset=True or restore=True all other args are ignored.
            With save=True other args can be present too (and become user defaults)
        """
        if reset or (restore and not self.myline):
            self.ln = {'width':1.0, 'color':'#000', 'opacity':None, 
                         'dash':None, 'css':None, 'style':None,
                         'svgstr':'stroke="#000" stroke-width="1.0" '}  # set explicitly
            self.myline = None
            return
        elif not self.svg['active']:         # but allow hard resets (above)
            raise RuntimeError('setline: no svg graphic begun yet')
        elif restore and self.myline:
            self.ln = self.myline.copy()   # svgstr in there
            return
        width = self.ln['width']      if not width else width
        color = self.ln['color']      if not color else color
        opacity = self.ln['opacity']  if opacity == 'ns' else opacity   
        opacity = None if opacity == 1.0 else opacity      # rewrite 1.0 to None 
        dash  = self.ln['dash']       if dash  == 'ns' else dash
        css   = self.ln['css']        if css   == 'ns' else css
        style = self.ln['style']      if style == 'ns' else style
        self.ln = {'width':width, 'color':color, 'opacity':opacity, 
                     'dash':dash, 'css':css, 'style':style}

        # build svgstr to be output into each svg line drawing element (see also svgstr above)
        svgstr = ''
        if css:     svgstr += f'class="{css}" '    # first for rendering reasons?
        if style:   svgstr += f'style="{style}" '               # ditto
        svgstr += f'stroke="{color}" '             # always required 
        svgstr += f'stroke-width="{width}" '       # always required 
        if opacity: svgstr += f'stroke-opacity="{opacity}" '    
        if dash:    svgstr += f'stroke-dasharray="{dash}" '
        self.ln['svgstr'] = svgstr
        if save:
            self.myline = self.ln.copy()
        return 
    
    
    def settext(self, reset=False, ptsize=None, color=None, opacity='ns', anchor='ns', 
               rotate='ns', adjust='ns', css='ns', style='ns', save=False, restore=False):
        """ Set text attributes for text rendering. They stay in effect until (re)specified.
            'ns' indicates 'not specified' where None is a valid specification.
            See setline above for behavior of reset, save, and restore.
        """   
        if reset or (restore and not self.mytext):
            self.text = {'ptsize':10, 'height': 13.89, 'color':'#000', 'opacity':None,
                     'anchor':None, 'rotate':None, 'adjust':None, 
                     'css':None, 'style':None, 'svgstr':'font-size="10pt" '} 
            return
        elif not self.svg['active']:
            raise RuntimeError('settext: no svg graphic begun yet')
        elif restore and self.mytext:
            self.text = self.mytext.copy()   # svgstr in there
            return
        anchor = 'middle' if anchor == 'center' else anchor            # user convenience
        ptsize = self.text['ptsize']    if not ptsize else ptsize
        color  = self.text['color']     if not color  else color
        opacity = self.text['opacity']  if opacity == 'ns' else opacity
        opacity = None if opacity == 1.0 else opacity    # rewrite 1.0 to None 
       
        anchor = self.text['anchor']    if anchor  == 'ns' else anchor   
        anchor = None if anchor == 'start' else anchor   # rewrite 'start' to None 
        rotate = self.text['rotate']    if rotate  == 'ns' else rotate   
        rotate = None if rotate == 0 else rotate         # rewrite 0 to None 
        adjust = self.text['adjust']    if adjust  == 'ns' else adjust
        adjust = None if adjust == (0,0) else adjust     # rewrite (0,0) to None
        adjust = None if adjust and len(adjust) != 2   else adjust     # ensure valid tuple
        css    = self.text['css']       if css     == 'ns' else css
        style  = self.text['style']     if style   == 'ns' else style
        self.text = {'ptsize':ptsize, 'color':color, 'opacity':opacity,
                     'anchor':anchor, 'rotate':rotate, 'adjust':adjust, 
                     'css':css, 'style':style}

        # build svgstr to be output into each svg text drawing element
        # (anchor, adjust, rotate are handled in the text functions)
        svgstr = self._textsvg(ptsize, color, opacity, css, style)

        self.text['height'] = (self.text['ptsize']/72.0)*100.0   # approx height (native)
        self.text['svgstr'] = svgstr
        if save:
            self.mytext = self.text.copy()
        return 


    def _textsvg(self, ptsize=10, color=None, opacity=None, css=None, style=None):
        """ build svg code chunk for certain text parms """
        svgstr = ''
        if css:     svgstr += f'class="{css}" '
        if style:   svgstr += f'style="{style}" '
        svgstr += f'font-size="{ptsize:.0f}pt" '        # required, svg fallback unreliable
        if color:   svgstr += f'fill="{color}" '
        if opacity: svgstr += f'fill-opacity="{opacity}" '
        return svgstr


    def set_misc(self, crossorigin=None):
        """ misc settings 
            crossorigin has to do with CORS and image(), see:
             https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/crossorigin
        """
        self.crossorigin = crossorigin
        return


    def test_pattern(self, width, height):
        """ render a test pattern grid showing native svg units """
        width = int(width); height = int(height)
        inc = int(getinc(0, height))
        self.setline(width=0.5, color='#494', dash='5,3')
        self.settext(ptsize=11, color='#d88') # style='font-family: sans-serif')
        curx = inc   # don't do zero twice
        while curx <= width:
            self.lin(curx, 0.0, curx, height)
            anc = 'middle' if curx < width else 'end'
            self.txt(curx, 0.0, str(curx), anchor=anc, adjust=(0,2) )
            curx += inc
        cury = 0
        while cury <= height:
            self.lin(0.0, cury, width, cury)
            adj = (3,2) if cury < height else (3,-10)
            self.txt(0.0, cury, str(cury), anchor='start', adjust=adj)
            cury += inc
        self.setline(width=1.0, color='#777', dash=None); 
        self.lin(0, 0, width, 0); 
        self.lin(0, 0, 0, height); 
        self.txt(width-10, height-20, 'DataDraw svg result\n'
                                    'Native coordinates', anchor='end')
        # self.txt(width-10, height-60, 'X<sup>2</sup> and O<sub>2</sub>', anchor='end')
        # self.settext(rotate=-90)
        # self.txt(width-100, 50, 'X<sup>2</sup> and O<sub>2</sub>')
        return 


    def findrange(self, value):
        """ check a value to find min, max """
        if not self.fr.is_active():
            self.fr.reset()
        self.fr.testval(value)
        return


    def findrange_result(self, nearest=None):
        """ report on the found numeric min, max """

        if nearest in ['year', '3month', 'month', 'week', 'day', 'hour', 'minute',
                       '12hour', '6hour', '4hour', '3hour', '30minute', '10minute']:
            rangedict = self.fr.result(nearest=1)   # arbitrary 
            return _finalize_dt_range(rangedict, nearest, self.weekday0)   # for date/times

        if nearest:
            try: 
                nearest = float(nearest)
            except: 
                raise ValueError(f"findrange_result: unrecognized nearest='{nearest}'")
        return self.fr.result(nearest)


    def catinfo(self, datarows, column=None, accumcol=None, nulls='ignore'):            # wrapper
        return  _catinfo(datarows, column, accumcol, nulls)          


    def numinfo(self, datarows, column=None, find_distrib=False, binsize=None, 
                          accumcol=None, find_percentiles=False):        # wrapper
        return  _numinfo(datarows, column, find_distrib, binsize, accumcol, find_percentiles)


    def lin(self, x1, y1, x2, y2, svgstr=None):
        """ draw line from x1,y1 to x2,y2 (native) """
        try: 
            testnum = float(x1) + y1 + x2 + y2
        except: 
            print(f'lin: expects numerics but got x1={x1} y1={y1} x2={x2} y2={y2} ..skipping')
            return
        # user can pass svgstr to override self.ln['svgstr']
        svgstr = self.ln['svgstr'] if not svgstr  else svgstr
        sx1 = self.outval(x1); sy1 = self.outval(y1, flip=True)
        sx2 = self.outval(x2); sy2 = self.outval(y2, flip=True)
        self.svg['out'] += f'<polyline points="{sx1},{sy1},{sx2},{sy2}" {svgstr} />\n'
        return 
    

    def txt(self, x, y, txt, anchor='ns', adjust='ns', rotate='ns', svgstr=None):
        """ render text at x,y (native) ... some overrides available ('ns'=not set) """
        txt = str(txt)
        try: 
            testnum = float(x) + y 
        except:
            print(f'txt: expects numeric x, y but got x={x} y={y} ..skipping')
            return
        adjust = self.text['adjust'] if adjust=='ns' else adjust
        if adjust:
            try:
                x += adjust[0]
                y += adjust[1]
            except:
                print(f"txt: expects 'adjust' as numeric tuple but got {adjust} ...ignoring")
        svgstr = self.text['svgstr'] if not svgstr  else svgstr
        sx = self.outval(x)
        sy = self.outval(y, flip=True)
        self.svg['out'] += f'<text x="{sx}" y="{sy}" {svgstr} '
        anchor = self.text['anchor'] if anchor=='ns' else anchor 
        if anchor and anchor in ['start', 'middle', 'end']:   
            self.svg['out'] += f'text-anchor="{anchor}" '
        rotate = self.text['rotate'] if rotate=='ns' else rotate
        if rotate and rotate != 0:
            self.svg['out'] += f'transform="rotate({rotate}, {sx}, {sy})" '
        txt = self._encode_special(txt, sx)   # handle embedded   \n  <sup>  <sub>
        self.svg['out'] += f'>{txt}</text>\n'
        return


    def _encode_special(self, txt, sx):
        """ rewrite txt w/ <tspan> for any embedded \n  <sup>  <sub> """
        len0 = len(txt)
        tspanstr = f'</tspan><tspan x="{sx}" dy="1.05em">'
        txt = txt.replace( "\n", tspanstr )    # encode embedded \n
        if len(txt) != len0: 
            txt = f'<tspan>{txt}</tspan>'   

        len0 = len(txt)
        slift = self.outval(self.text['height']*0.6)
        smallpt = int(self.text['ptsize']*0.7)
        tspanstr = f'<tspan dy="-{slift}" font-size="{smallpt}pt">'
        tspanstr2 = f'</tspan><tspan dy="{slift}" >'
        txt = txt.replace( "<sup>", tspanstr ).replace( "</sup>", tspanstr2 )  # <sup>
        drop = self.outval(self.text['height']*0.3)
        tspanstr = f'<tspan dy="{drop}" font-size="{smallpt}pt">'
        tspanstr2 = f'</tspan><tspan dy="-{drop}">'
        txt = txt.replace( "<sub>", tspanstr ).replace( "</sub>", tspanstr2 )  # <sub>
        if len(txt) != len0: 
            txt = txt + "</tspan>"  

        return txt


    def rect(self, x1, y1, x2, y2, color='#ddd', opacity=1.0, outline=False, rounded=None):
        """ rectangle, lower-left at x1,y1 and upper right at x2,y2 (native) """
        try: 
            testnum = float(x1) + y1 + x2 + y2
        except: 
            print(f'rect: expects numerics but got: x1={x1} y1={y1} x2={x2} y2={y2} ...skipping')
            return
        sx = self.outval(x1)   # convert args to what svg rect uses...
        sy = self.outval(y2, flip=True)
        wid = self.outval(x2-x1)
        hi = self.outval(y2-y1)
        self.svg['out'] += f'<rect x="{sx}" y="{sy}" width="{wid}" height="{hi}" '
        if rounded:
            if rounded == True:
                self.svg['out'] += 'rx="12" ry="12" '
            else: 
                self.svg['out'] += f'rx="{rounded[0]}" ry="{rounded[1]}" '
        self.svg['out'] += self._polysvg(color, opacity, outline) + '/>\n'
        return 


    def polygon(self, ptlist, color="#aaa", opacity=1.0, outline=False):
        """ render a polygon using ptlist (all coords native) """
        self.svg['out'] += '<polygon points="'
        for pt in ptlist:
            sx = self.outval(pt[0])
            sy = self.outval(pt[1], flip=True)
            self.svg['out'] += f'{sx},{sy} '
        self.svg['out'] += '" ' + self._polysvg(color, opacity, outline) + '/>\n'
        return


    def circle(self, x, y, diameter, color='#ddd', opacity=1.0, outline=False):
        """ render a circle (all coords native) """
        try: 
            testnum = float(x) + y + diameter
        except: 
            print(f'circle: expects numerics but got x={x} y={y} diameter={diameter} ...skipping')
            return
        x = self.outval(x)
        y = self.outval(y, flip=True)
        radius = self.outval(diameter/2.0)
        self.svg['out'] += f'<circle cx="{x}" cy="{y}" r="{radius}" '
        self.svg['out'] += self._polysvg(color, opacity, outline) + '/>\n'
        return


    def ellipse(self, x, y, width=100, height=30, color="#ddd", opacity=1.0, outline=False):
        """ render an ellipse (all coords native)  """
        try: 
            testnum = float(x) + y + height + width
        except:
            print(f'ellipse: expects numerics but got x={x} y={y} width={width} height={height} ...skipping')
            return
        x = self.outval(x)
        y = self.outval(y, flip=True)
        rx = self.outval(width/2.0)
        ry = self.outval(height/2.0)
        self.svg['out'] += f'<ellipse cx="{x}" cy="{y}" rx="{rx}" ry="{ry}" '
        self.svg['out'] += self._polysvg(color, opacity, outline) + '/>\n'
        return

    
    def image(self, href, x, y, width=None, height=None, opacity=1.0, par=True):
        """ render an image with upper-left at x, y (all coords native) 
            par = preserve aspect ratio, see url below for details.  
            Reference: https://developer.mozilla.org/en-US/docs/Web/SVG/Element/image
        """
        x = self.outval(x)
        y = self.outval(y, flip=True)
        self.svg['out'] += f'<image href="{href}" x="{x}" y="{y}" opacity="{opacity}" '
        if width!=None:
            width = self.outval(width)
            self.svg['out'] += f'width="{width}" '
        if height!=None:
            height = self.outval(height)
            self.svg['out'] += f'height="{height}" '
        if par == False:
            self.svg['out'] += f'preserveAspectRatio="none" '  # don't preserve aspect ratio
            # https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio 
        if self.crossorigin:
            self.svg['out'] += f'crossorigin="{self.crossorigin}" '
        self.svg['out'] += '/>\n'
        return


    def gtag(self, mode, tooltip=None, astring=None, gstring=''):
        """ produce a <g> tag for tooltips, linkouts, etc. """
        svgout = ''
        if mode == 'begin':
            # if programmer error, close out previous gtag
            if self.gstate == 2: svgout += '</a>'
            if self.gstate != 0: svgout += '</g>\n'
    
            self.gstate = 1
            svgout += f'<g {gstring} >\n'
            if tooltip:
                svgout += f'<title>{tooltip}</title>\n'
            if astring:
                self.gstate = 2
                svgout += f'<a {astring} >\n'
        elif mode == 'end':
            if self.gstate == 2: svgout += '</a>'
            if self.gstate != 0: svgout += '</g>\n'
            self.gstate = 0
        else:
            raise RuntimeError(f'gtag: unrecognized mode: {mode}')
        self.svg['out'] += svgout
        return


    def setspace(self, axis, svgrange=None, datarange=None, categorical=None, 
                     reverse=False, allint=False, log=False):
        """ set up scaling in either X or Y.
            svgrange is a tuple of (min, max) and always required.
            datarange is either a tuple of (min, max) or dict from findrange_result() 
              and its presence indicates the space will be numeric scaled.
            categorical is either a list of category names or dict keyed on category
              names, and its presence indicates the space will be categorical.
        """
        if not self.svg['active']:
            raise RuntimeError('setspace: no svg graphic begun yet')
        axis = axis.upper()
        try:
            iax = self.iax[axis]
        except KeyError:
            raise ValueError('setspace: axis parm must be either X or Y')
        try:             # see if svgrange is a dict (ie. doing multipanel)
            poslo = svgrange['poslo']
            poshi = svgrange['poshi']
        except:
            try:
                poslo = int(svgrange[0])
                poshi = int(svgrange[1])
            except:
                raise ValueError(f'setspace {axis}: expected svgrange as tuple of int')
        if poslo >= poshi:
            raise RuntimeError(f'setspace {axis}: invalid svgrange, must be (low, high)')

        if categorical:
            if type(categorical) is dict:
                catlist = list(categorical.keys())
            else:
                catlist = categorical
            self.space[iax] = catspace(iax, catlist, poslo, poshi, reverse=reverse)
        else:
            try:
                axmin = datarange['axmin']  # if this works, it's a findrange result dict
                axmax = datarange['axmax']
                datarange = (axmin, axmax)
            except:
                pass
            self.space[iax] = numspace(iax, datarange, 
                                 poslo, poshi, log=log, reverse=reverse, allint=allint)
        return



    def nx(self, dataval):
        """ return native X coordinate for the given x coord in data space """
        return self.nu('X', dataval)

    def ny(self, dataval):
        """ return native Y coordinate for the given y coord in data space """
        return self.nu('Y', dataval)

    def natpair(self, x, y):
        """ return a tuple of native (x,y) for the given data space (x,y) """
        nx = self.nu('X', x)
        ny = self.nu('Y', y)
        return nx, ny

    def natdist(self, axis, datadist):
        """ for a distance in a numeric data space, convert to a distance in native units """
        return  self.nu(axis, datadist) - self.nu(axis, 0.0)

    def inrange(self, axis, dataval):   # was named inspace()
        """ if dataval within the axis space return True otherwise False """
        natval = self.nu(axis, dataval) 
        if natval >= self.nmin(axis) and natval <= self.nmax(axis): 
            return True
        return False

    """ return plot are min or max in native or data coordinates """
    def nmin(self, axis): 
        return self.space[self.iax[axis]]['poslo']

    def nmax(self, axis): 
        return self.space[self.iax[axis]]['poshi']

    def dmin(self, axis): 
        return self.space[self.iax[axis]]['min']

    def dmax(self, axis): 
        return self.space[self.iax[axis]]['max']


    def outval(self, val, flip=False):   # was str2f
        """ produce an output svg numeric coordinate by (maybe) Y-flipping, and rounding """
        if flip:                                 # always done just before outputting svg
            val = self.svg['height'] - val
        return f'{val:.2f}'.replace('.00','')


    def nu(self, axis, dataval):
        """ for a location in data space (or 'min' or 'max') return native coordinate."""
        if dataval == 'min': 
            return self.nmin(axis)
        elif dataval == 'max': 
            return self.nmax(axis)

        iax = self.iax[axis]
        try:
            scaletype = self.space[iax]['scaletype'] 
        except:
            raise RuntimeError(f'attempt to scale in {axis} but setspace not yet done')
    
        if scaletype == 'categorical' and type(dataval) is str:  
            # BTW categories can also be referenced by integer
            # eg. 0.5 == 1st category,  1.5 == 2nd category etc.
            try:  
                ival = self.space[iax]['catdict'][dataval]
            except:  
                raise ValueError(f'encountered unrecognized {axis} '
                                 f'category term: {dataval}')
            dataval = ival + 0.5

        minval = self.space[iax]['min']
        scalefactor = self.space[iax]['scalefactor']
        rev = self.space[iax]['reverse']
        try:
            sval = (dataval - minval) * scalefactor
            if scaletype[:3] != "log" and not rev:
                return self.nmin(axis) + sval
            elif scaletype[:3] != "log" and rev:
                return self.nmax(axis) - sval
            else:
                pass
        except: 
            raise ValueError(f'numeric {axis} data value expected but got: {dataval}')

        if scaletype[:3] != 'log':      # if we reach here, doing log...
            raise RuntimeError(f'invalid scaletype in {axis}: {scaletype}')
        try:    
            if dataval <= 0.0:   # allow, tag it to minima
                return self.nmin(axis)
            plus1 = 1.0 if scaletype == "log+1" else 0.0
            sval = (nicelog(dataval+plus1) - nicelog(minval+plus1)) * scalefactor
            if scaletype[:3] == 'log' and not rev:        
                return self.nmin(axis) + sval
            elif scaletype[:3] == "log" and rev:           
                return self.nmax(axis) - sval
        except:
            raise ValueError(f'log-legal data value expected in {axis} but got: {dataval}')
        return


    def catranges(self, axis):
        """ return a dict of the native (min, max) for all currently defined categories """
        iax = self.iax[axis]
        try:
            ncats = len(self.space[iax]['catdict'])
        except:
            raise RuntimeError(f'catranges: no categories are currently defined in {axis}')
        ranges = {}
        for key in self.space[iax]['catdict']:   # originally defined dict order persists
            ival = self.space[iax]['catdict'][key]
            min = self.nu(axis, ival)
            max = self.nu(axis, ival+1)
            ranges[key] = (min, max)
        return ranges


    def axis(self, ax, axisline=True, inc=None, tics=None, stubs=True, grid=False, 
             loc=None, stubformat=None, divideby=None, stubcull=None, stublist=None, 
             stubrotate=None, stubrange=None, comma000=False, stubadjust=None, 
             stubanchor=None, dateconvert=None):
        """ render an axis scale (X or Y) """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'axis: setspace for both X and Y not done yet.')
        try:
            iax = self.iax[ax]           # 0=X  1=Y
        except KeyError:
            raise ValueError(f'axis: first arg must be either "X" or "Y"')

        # incoming parms...
        locterm = loc
        stubcull = None if (stubcull and type(stubcull) is not int) else stubcull
        if inc:   
            self.space[iax]['inc'] = inc     # user's explicit inc
        if stubformat and stubformat[0] != '%': 
            stubformat = '%'+stubformat      # in case newer specifiers eg '.0f' are used
        stadj = stubadjust  # tuple of 2 numerics

        # extent of entire axis in native units...
        poslo = self.space[iax]['poslo'] 
        poshi = self.space[iax]['poshi']
     
        # get some loc-related position vals... afterwards, loc is either 'min' or 'max'...
        loc, axloc, ticend, gridend = axispos(locterm, iax, tics, self.space)

        # axis line...
        if axisline and iax == 0:
            self.lin(poslo, axloc, poshi, axloc)
        elif axisline and iax == 1:
            self.lin(axloc, poslo, axloc, poshi)
     
        # extent of axis in data units...
        if stubrange and len(stubrange)==2:
            valstart = stubrange[0]             # tuple of 2 numerics
            valend = stubrange[1]
        elif stubrange!=None:
            valstart = stubrange                # just the start value, not a tuple
            valend = self.space[iax]['max']
        else:
            valstart = self.space[iax]['min']   # use entire axis
            valend = self.space[iax]['max']
        valinc = self.space[iax]['inc']
     
        # do any regularly spaced tics and grid lines....
        if (tics or grid) and not stublist:
            val = valstart
            prevdrawn = -999999.0
            while val <= valend:
                if not self.inrange(ax, val) or \
                  (stubcull and fabs(self.nu(ax, val)-prevdrawn) < stubcull):  
                    val += valinc    
                    continue
                self._render_ticline(iax, axloc, val, tics, ticend, grid, gridend)
                prevdrawn = self.nu(ax, val) 
                val += valinc

        if stubs:
            # prepare to render stubs text...
            rot0 = self.text['rotate']                # save current setting 
            self.text['rotate'] = stubrotate if stubrotate else 0    # rotate if needed

            xadj, yadj, anc = stubpos(iax, loc, self.text['rotate'], self.text['height'], tics)
            anc = stubanchor if stubanchor else anc  # override
                    
            # render stubs
            if stublist:        # list of val (or val,label pairs) for irregular placement
                prevdrawn = -999999.0
                for mem in stublist:
                    try:
                        val = float(mem[0])
                        txt = str(mem[1])
                    except:
                        val = float(mem)
                        txt = f'{val:g}'  # str(val)
                    if not self.inrange(ax, val):
                        continue
                    if stubcull and fabs(self.nu(ax,val)-prevdrawn) < stubcull:
                        continue
                    prevdrawn = self.nu(ax, val)
                    if iax == 0:
                        self.txt(self.nx(val)+xadj, axloc+yadj, txt, anchor=anc, adjust=stadj)
                    else:
                        self.txt(axloc+xadj, self.ny(val)+yadj, txt, anchor=anc, adjust=stadj)
                    if tics or grid:
                        self._render_ticline(iax, axloc, val, tics, ticend, grid, gridend)
    
            elif self.space[iax]['scaletype'] == 'categorical':   # categorical stubs
                stubformat = '%s' if not stubformat else stubformat
                for cat in self.space[iax]['catdict']:
                    if cat[:8] == '!spacer:':  
                        continue                             # skip spacers
                    if dateconvert and len(dateconvert)==2:  # tuple of 2 dtformats
                        utime = _intdt(cat, dateconvert[0])
                        outstr = _outdt(utime, dateconvert[1])
                    else:
                        outstr = stubformat % str(cat)
                    if iax == 0: 
                        self.txt(self.nx(cat)+xadj, axloc+yadj, outstr, anchor=anc, adjust=stadj)
                    else: 
                        self.txt(axloc+xadj, self.ny(cat)+yadj, outstr, anchor=anc, adjust=stadj)
 
            else:               # regular numeric stubs
                try:
                    deffmt = '%g' if valinc < 1.0 else '%.0f'
                    stubformat = deffmt if not stubformat else stubformat
                    prevdrawn = -999999.0
                    val = valstart
                    while val <= valend:
                        if not self.inrange(ax, val) or \
                          (stubcull and fabs(self.nu(ax, val)-prevdrawn) < stubcull):
                            val += valinc
                            continue
                        outval = val/divideby if divideby else val
                        if comma000:
                            outstr = f'{outval:,.0f}' # convenience op, comma-sep thousands
                        else:
                            outstr = stubformat % outval
                        if iax == 0: 
                            self.txt(self.nx(val)+xadj, axloc+yadj, outstr, anchor=anc, adjust=stadj)
                        else:
                            self.txt(axloc+xadj, self.ny(val)+yadj, outstr, anchor=anc, adjust=stadj)
                        prevdrawn = self.nu(ax, val) 
                        val += valinc
                except:
                    raise RuntimeError(f'axis ({ax}): numeric stubs error')
     
            self.text['rotate'] = rot0    # restore
        return 


    def _render_ticline(self, iax, axloc, val, tics, ticend, grid, gridend):
        """ draw one tic and/or grid line """
        # print(f'ticline  iax={iax}  grid={grid}  gridend={gridend}')
        if iax == 0:
            natx = self.nx(val)
            if tics:
                self.lin(natx, axloc, natx, ticend)
            if grid:
                self.lin(natx, axloc, natx, gridend)
        else:
            naty = self.ny(val)
            if tics:
                self.lin(axloc, naty, ticend, naty)
            if grid:
                self.lin(axloc, naty, gridend, naty)
        return


    def plotlabels(self, title=None, xlabel=None, ylabel=None, 
                      titlepos=5, xlabelpos=-40, ylabelpos=-60):
        """ convenient way to add title, axis labels to plot area """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'plotlabels: setspace for both X and Y not done yet.')
        natxmin = self.nmin('X'); natymin = self.nmin('Y')
        natxmax = self.nmax('X'); natymax = self.nmax('Y')
        midx = (natxmin+natxmax)/2.0
        midy = (natymin+natymax)/2.0
        if title:
            self.txt(natxmin, natymax+titlepos, title, anchor='start')
        if xlabel:
            self.txt(midx, natymin+xlabelpos, xlabel, anchor='middle')
        rot0 = self.text['rotate']  
        if ylabel:
            self.text['rotate'] = -90
            self.txt(natxmin+ylabelpos, midy, ylabel, anchor='middle')
        self.text['rotate'] = rot0   # restore
        return


    def plotbacking(self, color=None, outline=False, rounded=None, image=None):
        """ add outline, background color, background image to plot area """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'plotlabels: setspace for both X and Y not done yet.')
        if image:
            # load image to completely fill the scaled plot area (par=False)
            wid= self.nmax('X') - self.nmin('X')
            hi = self.nmax('Y') - self.nmin('Y')
            self.image(image, self.nmin('X'), self.nmax('Y'), width=wid, height=hi, par=False)
        if color or outline: 
            # color = '#fff' if not color else color  # if just outlining, don't fill 
            self.rect(self.nmin('X'), self.nmin('Y'), self.nmax('X'), self.nmax('Y'), 
                color=color, outline=outline, rounded=rounded)
        return

 
    def line(self, x1, y1, x2, y2):
        """ draw a line in data space """
        x1, y1 = self.natpair(x1, y1)
        x2, y2 = self.natpair(x2, y2)
        self.lin(x1, y1, x2, y2)
        return 


    def label(self, x, y, text, anchor='start', adjust=None):
        """ render text at x, y in data space """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'label: setspace for both X and Y not done yet.')
        # y -= self.text['height']*0.3   # this function always centers vert. around Y
        x, y = self.natpair(x, y)
        # always center vertically around Y (but not multiline text)....
        y -= self.text['height']*0.3     
        self.txt( x, y, text, anchor=anchor, adjust=adjust)
        return


    def rectangle(self, cx, cy, width, height, color="#afa", opacity=1.0, 
                  rounded=None, outline=False):
        """ draw rectangle with center at x,y and height, width.. all in data units.  """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'rectangle: setspace for both X and Y not done yet.')
        try:
            testnum = float(cx) + cy + width + height
        except:
            print(f'rectangle: expects numerics but got: '
                  f'center ({cx},{cy})  width={width} height={height} ...skipping')
            return
        x1 = cx-(width/2.0)
        y1 = cy+(height/2.0)  # plus here due to flippage
        x, y = self.natpair(x1, y1)
        sx = self.outval(x)   # convert args to what svg rect uses...
        sy = self.outval(y, flip=True)
        wid= self.outval(self.natdist('X', width))
        hi = self.outval(self.natdist('Y', height))
        self.svg['out'] += f'<rect x="{sx}" y="{sy}" width="{wid}" height="{hi}" '
        if rounded:
            if rounded == True:
                self.svg['out'] += f'rx="12" ry="12" '
            elif len(rounded) == 2:    # tuple of 2 numeric values
                self.svg['out'] += f'rx="{rounded[0]}" ry="{rounded[1]}" '
        self.svg['out'] += self._polysvg(color, opacity, outline) + '/>\n'
        return


    def arrow(self, x1, y1, x2=None, y2=None, direction=None, magnitude=None, 
           headlen=18, headwid=0.3, tiptype='solid', tipcolor='#888', opacity=1.0):
        """ draw an arrow in data space with non-tip end at x1,y1 and the tip end
            at x2,y2 (or else as defined using polar-style direction,magnitude)  
        """
        if (x2 == None or y2 == None) and not (direction and magnitude):
            print('arrow: problem with supplied args ...skipping')
            return
        x1, y1 = self.natpair(x1, y1)
        if x2: 
            x2, y2 = self.natpair(x2, y2)
        elif direction:
            x2, y2 = polar(x1, y1, direction, magnitude)
        # from here on we're in native units...
        self.lin(x1, y1, x2, y2)   
        ax1, ay1, ax2, ay2 = arrowhead(x1, y1, x2, y2, headlen, headwid)
        if tiptype == 'solid':
            ptlist = ( (x2,y2), (ax1,ay1), (ax2,ay2) )
            self.polygon(ptlist, color=tipcolor, opacity=opacity, outline=False)
        elif tiptype[:4] == 'line': 
            self.lin(x2, y2, ax1, ay1); 
            self.lin(x2, y2, ax2, ay2)
        elif tiptype[:4] == 'barb': 
            self.lin( x2, y2, ax1, ay1 ); 
        return 


    def _polysvg(self, color, opacity, outline):
        """ build svg code chunk for polygon (etc) fill and outline parms """
        if not color and not outline:
            print(f'rect: no color or outline specified so rect will not be visible')
        svgout = ''
        if outline: 
            svgout += self.ln['svgstr'] + ' '
        if color: 
            svgout += f'fill="{color}" '
        else:
            svgout += f'fill="none" '   # feb'23
        if opacity != 1.0: 
            # svgout += f'opacity="{opacity}" '
            svgout += f'fill-opacity="{opacity}" '
        return svgout


    def errorbar(self, x=None, y=None, erramt=None, ymin=None, ymax=None, tailsize=5, 
                        shift=0.0, horiz=False):
        """ render an error bar.  x is always specified.  
            Either y and erramt, or ymin and ymax, must be specified 
        """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'errorbar: setspace for both X and Y not done yet.')
        if x == None or (not (y!=None and erramt!=None) and not (ymin!=None and ymax!=None)):
            # we do   !=None   above because   if not 0   ... is true 
            print(f'errorbar: got x={x}  y={y} erramt={erramt}  ymin={ymin} ymax={ymax} ...skipping')
            return 
        if y:
            try: 
                ymin = y - erramt; 
                ymax = y + erramt;
            except: 
                print(f"errorbar: expects numerics but got y={y}, erramt={erramt} ...skipping")
                return
        try: 
            ymin = float(ymin); 
            ymax = float(ymax)
        except: 
            print(f"errorbar: expects numerics but got ymin={ymin} ymax={ymax} ...skipping")
            return
        f = tailsize/2.0
        if horiz: 
            natx = self.ny(x)
            natymin = self.nx(ymin)
            natymax = self.nx(ymax)
            # (shift is subtracted; intuitive for higher shift value to move it downward)
            self.lin(natymin, natx-shift, natymax, natx-shift)
            self.lin(natymin, (natx-shift)-f, natymin, (natx-shift)+f )
            self.lin(natymax, (natx-shift)-f, natymax, (natx-shift)+f )
        else:             
            natx = self.nx(x)
            natymin = self.ny(ymin)
            natymax = self.ny(ymax)
            self.lin( natx+shift, natymin, natx+shift, natymax )
            self.lin( (natx+shift)-f, natymin, natx+shift+f, natymin )
            self.lin( (natx+shift)-f, natymax, natx+shift+f, natymax )
        return 


    def bar(self, x=None, y=None, ybase=None, width=8, color="#afa", opacity=1.0, 
                  outline=False, shift=0.0, horiz=False, rounded=None):
        """ render a column bar """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'bar: setspace for both X and Y not done yet.')
        if x==None or y==None:  
            print(f'bar: got x={x} y={y} ...skipping')
            return 
        try: 
            y = float(y)
        except:  
            print(f"bar: expects numeric but got y={y} ...skipping")
            return
        if ybase==None:
            ybase = self.dmin('Y') if not horiz else self.dmin('X')
        try: 
            ybase = float(ybase)
        except:  
            print(f"bar: expects numeric but got ybase={ybase} ...skipping")
            return
        if ybase > y:
            ytmp = y; y = ybase; ybase = ytmp;  # downward bars 
        try:
            f = width/2.0
        except:
            print(f"bar: expects numeric but got width={width} ...skipping")
            return
        if horiz: 
            natx = self.ny(x)
            # (shift is subtracted; intuitive for higher shift value to move it downward)
            self.rect( self.nx(ybase), (natx-f)-shift, self.nx(y), (natx+f)-shift, 
                      color=color, opacity=opacity, outline=outline, rounded=rounded)
        else:             
            natx = self.nx(x)
            self.rect( (natx-f)+shift, self.ny(ybase), natx+f+shift, self.ny(y), 
                      color=color, opacity=opacity, outline=outline, rounded=rounded)
        return 


    def boxplot(self, info, x=None, width=16, color='#ccc', n_at_y=1.0, shift=0.0):
        """ render a vertical tukey box+whisker plot.  
            numinfo is dict returned by numinfo(percentiles=True)
        """
        if x == None:
            raise ValueError(f'boxplot: expects x but got x={x}')
        pctiles = info['percentiles']
        self.errorbar(x=x, ymin=pctiles['p5'], ymax=pctiles['p95'], shift=shift )
        self.bar(x=x, ybase=pctiles['p25'], y=pctiles['p75'], width=width, 
                outline=True, color=color, shift=shift)
        self.datapoint(x=x, y=pctiles['median'], diameter=6, 
                       color='#777', adjust=(shift,0))
        if n_at_y != None:
            self.label(x=x, y=n_at_y, text=f"N = {info['nvals']}", 
                        anchor='middle', adjust=(shift,0))
        return


    def datapoint(self, x=None, y=None, symbol='(vcircle)', diameter=10, color='#777', 
              opacity=0.7, adjust=None, nativecoords=False, stretch=None, backing=None):
        """ render a circle data point or character-based data point """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'datapoint: setspace for both X and Y not done yet.')
        if backing:    # do this first
            try:
                self.datapoint(x, y, symbol=backing['symbol'], diameter=backing['diameter'], 
                           color=backing['color'], adjust=adjust, nativecoords=nativecoords, 
                           opacity=opacity, stretch=stretch)  
                opacity = 1.0 # for foreground 
            except:
                print(f"datapoint: expecting dict with 3 elements 'symbol', 'color', "
                      f"and 'diameter' elements, but got backing={backing}... so skipping")
        try:
            if nativecoords:
                natx = x; naty = y;
            else:
                natx, naty = self.natpair(x, y)
        except:
            print(f'datapoint: got x={x} y={y} ...skipping')
            return 
        if adjust:
            try:
                natx += adjust[0]
                naty += adjust[1]
            except:
                print(f"datapoint: expects 'adjust' as numeric tuple but got {adjust} ...ignoring")
        cx = 0.0; cy = 0.0;
        if self.dpclustering:
            cx, cy, omit = dpcluster(self.dpclustering, natx, naty)  # get small x,y offsets 
            if omit:
                return

        diameter = diameter * 0.85 if symbol == '(circle-o)' else diameter  # calibrate
        diameter = diameter * 1.25 if symbol == '(square)' else diameter    # calibrate
        diameter = diameter * 1.22 if symbol[:8] == '(diamond' or symbol == '(star4)' else diameter
        
        try:
            symbol = htmlcode(symbol)
        except KeyError:
            pass
        if symbol[:8] == '(vcircle':    # svg circle 
            outline = True if symbol == '(vcircle-o)' else False
            if stretch and len(stretch)==2:
                self.ellipse(natx+cx, naty+cy, width=diameter*stretch[0], 
                 height=diameter*stretch[1], color=color, opacity=opacity, outline=outline)
            else:
                self.circle(natx+cx, naty+cy, diameter=diameter, color=color, 
                            opacity=opacity, outline=outline)
            return
        elif symbol[:6] == '(vrect':    # svg square/rectangle
            outline = True if symbol == '(vrect-o)' else False
            stretch = (1,1) if not stretch or len(stretch)!=2 else stretch
            half_h = diameter * stretch[0] * 0.5
            half_v = diameter * stretch[1] * 0.5
            self.rect((natx+cx)-half_h, (naty+cy)-half_v, 
                      (natx+cx)+half_h, (naty+cy)+half_v, 
                       color=color, opacity=opacity, outline=outline)
            return

        if symbol[:5] == '(img)':
            diameter *= 1.1
            adj = int(diameter/2)
            self.image(symbol[5:], x=(natx-adj)+cx, y=(naty+adj)+cy, width=diameter)
            return

        # otherwise render an html char construct or any text... (don't use settext() here)
        svgstr = self._textsvg(ptsize=diameter, color=color, opacity=opacity)
        txthi = (diameter/72.0)*100.0
        # (Y finetune... triangles look better placed a little higher)
        naty = naty-(txthi*0.22) if symbol[:5] == '&#965' and len(symbol)==7 else naty-(txthi*0.3)   
        self.txt(x=natx+cx, y=naty+cy, txt=symbol, anchor='middle', rotate=None, svgstr=svgstr)
        return 

 
    def setclustering(self, mode='surround', offset=1.0, tolerance=0.0, dampen=1, 
                       conform=True, jitter=None):
        """ set dpclustering parameters for datapoint() """
        if not mode or mode == 'off':
            self.dpclustering = None
            return
        if mode not in ['surround', 'omit_dups', 'left+right', 'rightward', 'leftward', 
                        'up+down', 'upward', 'downward']:
            print(f"setclustering: warning, mode='{mode}' unrecognized, using 'surround'")
            mode = 'surround'
        try: 
            testnum = float(offset) + tolerance + dampen
        except: 
            print(f'setclustering: expecting numerics, got offset={offset}  '
                       'tolerance={tolerance}  dampen={dampen} so falling back to defaults')
            offset=1.0; tolerance=0.0; dampen=1;
        jitter = 0.5 if mode == 'surround' and not jitter else jitter
        jitter = 0.0 if not jitter else jitter
        self.dpclustering = {'mode':mode, 'offset':offset, 'tol':tolerance, 'jitter':jitter,
                          'dampen':dampen, 'conform':conform, 'ndup':0, 'prevx':0.0, 'prevy':0.0}
        return 


    def curvebegin(self, x=None, y=None, y2=None, fill=None, fillopacity=0.7, 
                         stairs=False, onbadval='bridge', shift=None):
        """ begin a lineplot curve or filled band.  The first (x,y) can optionally
            be supplied here... if not it's considered a 'gap' and the curve/fill 
            is begun on the subsequent curvenext() call.
        """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'curvebegin: setspace for both X and Y not done yet.')
        natx = self.nx(x)   if x!=None else None 
        naty = self.ny(y)   if y!=None else None 
        naty2 = self.ny(y2) if y2!=None else self.nmin('Y')
        if stairs and fill:
            print('curvebegin: sorry, fill not supported with stairs=True')
            fill = None
        self.curve = {'natx':natx, 'naty':naty, 'naty2':naty2, 
                      'stairs':stairs, 'fill':fill, 'fillopacity':fillopacity, 
                      'onbadval':onbadval, 'shift':shift}
        return 
    
    
    def curvenext(self, x=None, y=None, y2=None):
        """ continue a lineplot curve or filled band """
        if not (self.space[0]['scaletype'] and self.space[1]['scaletype']):
            raise RuntimeError(f'curvenext: setspace for both X and Y not done yet.')
    
        y2 = self.dmin('Y') if y2==None else y2
        sh = 0.0 if not self.curve['shift'] else self.curve['shift']

        if self.curve['naty']==None or self.curve['natx']==None:     
            gapping = True    # previous pt was N/A, render the gap now
        else:
            gapping = False
            prevnatx = self.curve['natx']
            prevnaty = self.curve['naty']
            prevnaty2 = self.curve['naty2']
        try:
            naty = self.ny(y)
        except:
            y = None
        try:
            natx = self.nx(x)   # x and y need to be tried separately here
        except:
            x = None
        if y==None or x==None:   # unplottable (n/a), bridge it or render a gap
            if self.curve['onbadval'] == 'gap' and not gapping: 
                if self.curve['stairs'] and x!=None:  # extend to where Y becomes N/A
                    self.lin(prevnatx+sh, prevnaty, natx+sh, prevnaty)
                self.curve['natx'] = self.curve['naty'] = self.curve['naty2'] = None
                return
            else:
                return

        self.curve['natx'] = self.nx(x)
        self.curve['naty'] = self.ny(y)
        self.curve['naty2'] = naty2 = self.ny(y2)
        if gapping:
            return
        if self.curve['fill']:
            color = self.curve['fill']
            opacity = self.curve['fillopacity']
            startingpt = (prevnatx+sh, prevnaty)
            self.polygon((startingpt, (natx+sh, naty), (natx+sh, naty2), 
               (prevnatx+sh, prevnaty2), startingpt), color=color, opacity=opacity)
        elif self.curve['stairs']:
            self.lin(prevnatx+sh, prevnaty, natx+sh, prevnaty)
            self.lin(natx+sh, prevnaty, natx+sh, naty)
        else: 
            self.lin(prevnatx+sh, prevnaty, natx+sh, naty)
        return 
    

    def pieslice(self, pctval=None, startval=0.0, color='#ccc', outline=False, opacity=1.0, 
                     placement='right', showpct=False ):
        """ render a piegraph slice.   pctval controls size of slice and is 0.0 to 1.0.
            startval controls where (radially) the slice "starts" and is also 0.0 to 1.0.
        """
        if pctval==None or pctval <= 0.0 or pctval > 1.0:
            print(f'pieslice: expects numeric 0.0 to 1.0 but got pctval={pctval} ...skipping')
            return 
    
        if startval == None or startval < 0.0 or startval > 8:
            print(f'pieslice: expects numeric 0.0 to 8.0 but got startval={startval} ...skipping')
            return 
    
        twopi = 6.28319
        halfpi = 1.5707963
        boxw = self.nmax('X') - self.nmin('X') 
        boxh = self.nmax('Y') - self.nmin('Y')
        radius = boxw/2.0 if boxw < boxh  else boxh/2.0
        cx = self.nmin('X')+radius if placement == 'left'  else self.nmax('X')-radius; 
        cy = self.nmin('Y')+(boxh/2.0)   
    
        theta = (-startval * twopi) + halfpi    # starting theta
        endtheta = theta - (pctval*twopi)       # ending theta
        txttheta = (theta+endtheta)/2.0         # for placing text percentage, if needed
    
        # build a list of points, first the two straight edges then the curved edge..
        pts = []
        pts.append((cx+(radius*cos(endtheta)), cy+(radius*sin(endtheta)))) 
        pts.append((cx, cy))
        pts.append((cx+(radius*cos(theta)), cy+(radius*sin(theta))))  
        while theta > endtheta:
            theta -= 0.03 
            pts.append((cx+(radius*cos(theta)), cy+(radius*sin(theta))))

        self.polygon(pts, color=color, outline=outline, opacity=opacity)
    
        if showpct:
            txtrad = radius * 0.7
            showpct = '%.0f' if showpct == True else showpct
            pctstr = showpct % (pctval*100.0)
            tx = cx+(txtrad*cos(txttheta))
            ty = cy+(txtrad*sin(txttheta))
            self.txt(tx, ty, pctstr+'%', anchor='middle')
    
        return 
    
    
    def legenditem(self, sample='square', label=None, color='#777', 
                 ewidth=None, symbol=None, diameter=None, opacity=0.7, backing=None):
        """ define a legend entry, to be rendered later using legendrender().
            ewidth= explicitly set legend entry's width (legend format='across' only)
            symbol= is used only with sample='symbol' and is required then; 
                    diameter= opacity= and backing= are used optionally only w/ symbol.
        """
        if not label:
            print('legenditem: expects label arg, none given ...skipping')
            return
        if not ewidth:   # make a rough guess of line length
            txthi = self.text['height']
            if label.find('\n') >= 0:
                ewidth = (label.find('\n')+1) * txthi*0.6     # contains newline(s), use 1st line
            else:
                ewidth = len(label) * txthi*0.6               # no newlines present

        if sample in ['square', 'circle']:
            self.leg.append({'sample':sample, 'label':label, 'color':color, 'ewidth':ewidth})
        elif sample == 'symbol':
            symbol = '(vcircle)' if not symbol else symbol 
            self.leg.append({'sample':'symbol', 'label':label, 'symbol':symbol, 
                             'color':color, 'ewidth':ewidth, 'diameter':diameter,
                             'opacity':opacity, 'backing':backing})
        elif sample == 'line':
            self.leg.append({'sample':'line', 'label':label, 'svgstr':self.ln['svgstr'], 
                             'ewidth':ewidth})
        else:
            print(f"legenditem: unrecognized sample='{sample}', skipping.")
        # line+symbol?
        return 
    
    
    def legendrender(self, location='top', format='down', sampsize=8, lnlen=20, title=None, 
                       adjust=(0,0), outline=False, rounded=None, sep=0):
        """ render the legend using entries defined earlier """
        if len(self.leg) == 0:
            print('legendrender: no legend entries defined ...skipping')
            return
        try:
            testval = adjust[0] + adjust[1]
        except:
            print(f'legendrender: expects tuple of two numerics but got adjust={adjust} ...ignoring')
            adjust = (0,0)
        txthi = self.text['height']
    
        if location not in ['top', 'bottom', 'in_adjust']:
            print(f"legendrender: warning, location='{location}' unrecognized, using 'top'")
            location = 'top'
        if location == 'top': 
            xpos = self.nmin('X')+5+adjust[0]
            ypos = (self.nmax('Y')-txthi)+adjust[1]
        elif location == 'bottom': 
            xpos = self.nmin('X')+5+adjust[0]
            ypos = self.nmin('Y')+3+adjust[1]
        elif location == 'in_adjust':
            xpos = adjust[0]
            ypos = adjust[1]
    
        if format == 'down':
            samparea_w = 10
            for row in self.leg:   # see if any line samps (line samps need wider samparea_w)...
                if row['sample'] == 'line':
                    samparea_w = lnlen; 
                    break   
    
        halfln = txthi*0.3
        x = xpos; y = ypos
        if title: 
            nlines = title.count('\n')+1
            self.txt(x, y, title) 
            y -= txthi * nlines * 1.1
        for row in self.leg:
            if format in ['across', 'parkinglot']:   # line samps need wider samparea...
                samparea_w = lnlen if row['sample'] == 'line'  else sampsize+2

            sx = x+(samparea_w*0.5)

            if row['sample'] == 'circle': 
                self.circle(sx, y+halfln, sampsize+1, 
                             color=row['color'], outline=outline)
            elif row['sample'] == 'square': 
                ssx = x+(samparea_w*0.25)  # (rect doesn't center like the others)
                self.rect(ssx, y, ssx+sampsize, y+(sampsize), color=row['color'], 
                              outline=outline, rounded=rounded)
            elif row['sample'] == 'symbol':
                # (legend could have same symbol different sizes)
                diam = row['diameter'] if row['diameter'] else sampsize+2
                self.datapoint(sx, y+halfln, symbol=row['symbol'], color=row['color'], 
                              diameter=diam, nativecoords=True, opacity=row['opacity'],
                              backing=row['backing'])
            elif row['sample'] == 'line': 
                self.lin(x, y+halfln, x+samparea_w, y+halfln, svgstr=row['svgstr'])

            if format == 'parkinglot':
                adjust = (-3, sampsize*0.8 ) 
                rotate = -35
            else:
                adjust = (0,0)
                rotate = None
            self.txt( x+samparea_w+5, y, row['label'], adjust=adjust, rotate=rotate)

            nlines = row['label'].count('\n')+1
            if format == 'down': 
                y -= (txthi * nlines * 1.1) + sep
            elif format == 'across': 
                x += row['ewidth'] + samparea_w + sep
            elif format == 'parkinglot': 
                x += 30+samparea_w+sep
    
        self.leg = []  # reset
        return 


    def setdt(self, dtformat=None, weekday0=None, fymonth1=None):
        """ make date/time-related settings. For python format codes:
            https://docs.python.org/2/library/datetime.html#strftime-and-strptime-behavior
            (note that 'zero-padded' refers to output only; parsing can handle eg. 3/4/2015)
            For weekday0, 0=Monday; use -1 or 6 to get Sunday, etc.
        """
        self.dtformat = dtformat if dtformat else self.dtformat
        self.weekday0 = (weekday0+7)%7 if weekday0 else self.weekday0
        self.fymonth1 = fymonth1 if fymonth1 else self.fymonth1
        return 

    
    def intdt(self, dtstr):
        return _intdt(dtstr, self.dtformat)


    def outdt(self, utime):
        return _outdt(utime, self.dtformat)


    def dtdiff(self, dt1, dt2, outunits='days'):
        return _dtdiff(dt1, dt2, outunits, self.dtformat)


    def datestubs(self, rangedict, inc=None, crossings=False, dtformat=None, terse=False):
        dtformat = '' if not dtformat else dtformat    # eg. to do tics only
        if dtformat == '':
            print(f'datestubs notice: dtformat={dtformat} so stubs will be blank')
        return _datestubs(rangedict, inc, crossings, dtformat, terse, 
                            self.weekday0, self.fymonth1)

