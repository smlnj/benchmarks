import math
import random

# non-mutable globals...

halfpi = 1.5707963

html_symbols = { '(circle)': '&#11044;', '(circle-o)':'&#9711;',
              '(triangle)':'&#9650;', '(triangle-o)':'&#9651;', '(dtriangle)':'&#9660;',
              '(square)': '&#11200;', '(square-o)':'&#8414;',
              '(diamond)':'&#11201;', '(diamond-o)':'&#9671;',
              '(star4)': '&#128966;', '(spokes5)':'&#128944;', '(spokes8)':'&#128956;'}
              # more: https://www.htmlsymbols.xyz/geometric-symbols
              #       https://www.htmlsymbols.xyz/star-symbols

clust_xofs = (0,0,4, 0,-4,4,-4,-4, 4, 0,-6,0,6, 4,-8,4,8,-4,-8,-4, 8, 0,
                       0,10,-10, 4,  4,10,-10,-4, -4,10,-10, 8,-8,-8, 8)
clust_yofs = (0,4,0,-4, 0,4,-4, 4,-4,-6, 0,6,0,-8, 4,8,4,-8,-4, 8,-4,10,-10, 0,
                             0,10,-10, 4,  4,10,-10,-4, -4, 8,-8, 8,-8)


""" Misc functions not visible to application developer.  Some have wrappers in sds.py, 
    if so, arg defaults are typically set in the wrapper, not here.
"""

def htmlcode(symbolname):
    return html_symbols[symbolname]


def _write_svgfile(svgcode, filename):
    """ write svg to a file with proper headers """
    fp = open(filename, 'w')
    print( '<?xml version="1.0" encoding="utf-8"?>', file=fp )
    print( '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" '
           '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">', file=fp )
    print(svgcode, file=fp )
    fp.close()
    return


def catspace(iax, catlist, poslo, poshi, reverse=False):
    """ set up a categorical X or Y space 
        :param iax:     0=X  1=Y
        :param catlist: list of observed category terms (must be a list)
        :param poslo, poshi:  range in native units
        :param reverse: if True axis runs hi to low from origin
    """
    scalefactor = (poshi-poslo)/ len(catlist)

    # in Y, flip the sense of 'reverse' since they run top to bottom by default
    if iax == 1:
        reverse = True if not reverse else False

    # build catdict...
    catdict = {}
    icount = 0
    for cat in catlist:
        if cat == '':
            cat = f'!spacer:{icount}'
        catdict[cat] = icount
        icount += 1
    return {'scaletype':'categorical', 'scalefactor':scalefactor, 'min':0, 'max':icount,
            'inc':1, 'catdict':catdict, 'reverse':reverse,
            'poslo':float(poslo), 'poshi':float(poshi)}


def numspace(iax, datarange, poslo, poshi, log=None, reverse=False, allint=False):
    """ set up a numeric X or Y space (linear or log) and return a dict of parameters.
        :param iax:     0=X  1=Y
        :param datarange:  data range as a tuple (low,high) 
        :param poslo, poshi:  range in native units
        :param reverse: if True axis runs hi to low from origin
        :param allint:  if True everything is integers
    """
    axis = ['X', 'Y']
    datarange = (0,100) if not datarange else datarange    # fallback for eg. pie graphs
    log = 'log' if log == True else log   # handle log passed as True
    try:    # see if datarange is in a dict as provided by findrange() or columninfo()
        axmin = datarange['axmin']
        axmax = datarange['axmax']
    except:
        try:
            axmin = float(datarange[0])
            axmax = float(datarange[1])
        except:
            raise ValueError(f'setspace {axis[iax]}: unusable datarange: {datarange}')
    if axmin > axmax:
        raise RuntimeError(f'setspace {axis[iax]}: datarange magnitude error')

    if log in ['log', 'log+1']:
        if axmin < 0.0:
            raise RuntimeError(f'setspace {axis[iax]}: log scaling: no negative values')
        scaletype = 'log'
    else:
        scaletype = 'numeric'

    # compute the tic increment...
    inc = getinc(axmin, axmax)
    if inc < 1.0 and allint:
        inc = 1.0

    # compute scalefactor...
    natrange = poshi - poslo
    if log == "log":
        datrange = nicelog(axmax) - nicelog(axmin)
    elif log == "log+1":
        datrange = nicelog(axmax+1.0) - nicelog(axmin+1.0)
    else:
        datrange = axmax - axmin
    scalefactor = natrange / datrange

    return {'scaletype':scaletype, 'scalefactor':scalefactor, 'reverse':reverse,
        'min':float(axmin), 'max':float(axmax), 'inc':inc,
        'poslo':float(poslo), 'poshi':float(poshi)}


def getinc(minval, maxval):
    """ find a reasonable inc (always float) for the given numeric range...
        thanks to Dan Pelleg peldan@yahoo.com  who provided this algorithm """
    diff = math.fabs( maxval - minval )
    h = diff / 10.0
    try:
        log10h = math.floor( math.log10(h) )
        mult = math.pow(10.0, log10h)
        mantissa = h / mult
    except:
        print(f'failed to compute inc, using non-ideal fallback inc={diff/10.0}')
        return diff / 10.0   
    if mantissa < 2.0:   
        inc = 2.0 * mult
    elif mantissa < 5.0: 
        inc = 5.0 * mult
    else:  
        inc = 10.0 * mult
    return inc


def nicelog(dataval):
    """ wrapper for math.log that is lenient with dataval=0.0 """
    if dataval == 0.0:
        return 0.0 # allow, tag it to minima
    return math.log(dataval)


def axispos(locterm, iax, tics, space):
    """ parse axis loc term eg. 'min', 'max', 'min-8' 'max+11', 'left', 'bottom' etc.
        (usual is 'min' or 'left' / 'bottom').  Also return some related position vals.
    """
    locterm = 'min' if not locterm else locterm.replace(' ','')
    locterm = locterm.replace('+',' +').replace('-',' -')
    chunks = locterm.split()
    loc = chunks[0]
    try:
        adjust = int(chunks[1])
    except:
        adjust = 0

    # set some positions based on user's axis location...
    loc = 'min' if loc not in ['min', 'max', 'left', 'bottom', 'right', 'top'] else loc
    oiax = 1 if iax == 0 else 0  # the other axis

    if loc in ['min', 'left', 'bottom']:
        axloc = space[oiax]['poslo'] + adjust
        ticend = axloc+tics if tics else axloc
        gridend = space[oiax]['poshi']
        loc = 'min'
    elif loc in ['max', 'right', 'top']:
        axloc = space[oiax]['poshi'] + adjust
        ticend = axloc+tics if tics else axloc
        gridend = space[oiax]['poslo']
        loc = 'max'

    return loc, axloc, ticend, gridend


def stubpos(iax, loc, rot, txthi, tics):
    """ do some finetuning of stubs position, factoring in any slant, etc. 
        Return 3 items, see below.
    """
    xadj = 0
    yadj = 0
    anc = 'middle'  # sanity fallback
    ticng = tics if tics and tics < 0 else -2
    ticps = tics if tics and tics > 0 else 2
    if iax == 0:
        if loc == 'min':
            if rot == 0:
                anc = 'middle'
                yadj = -(txthi)+ticng if loc == 'min' else 3   
            elif rot > 0 and rot <= 90:
                anc = 'start'
                xadj = -5
                yadj = ticng - 2
            elif rot < 0 and rot >= -90:
                anc = 'end'
                xadj = 4
                yadj = -(txthi*0.5)+ticng
        elif loc == 'max': 
            if rot == 0:
                anc = 'middle'
                yadj = ticps+2
            elif rot > 0 and rot <= 90:
                anc = 'end'
                yadj = ticps
            elif rot < 0 and rot >= -90:
                anc = 'start'
                yadj = ticps
    elif iax == 1:
        yadj = txthi * -0.3         # always adjust to center Y stubs on tics (vertically)
        if loc == 'max':
            anc = 'start'
            xadj = ticps+2
        else:
            anc = 'end'   # usual case
            xadj = ticng -2
    return xadj, yadj, anc


class FindRange:
    """ find nice numeric axis min and max. Call testval() repeatedly
        for each data value, then call result() to return a results dict.
        If doing stacked bars caller should pass the stack sum to testval().
        If doing +/- error bars caller should call testval twice, first for 
        value+err then again for value-err.
    """
    def __init__(self):
        self.active = False
        self.reset()
        return

    def is_active(self):
        return self.active

    def reset(self):
        self.nvals = self.nbadvals = 0
        self.allint = self.allpos = self.allneg = True;
        self.minval =  9.99e+99
        self.maxval = -9.99e+99

    def testval(self, val):
        """ test a value against current min and max. """
        self.active = True
        try: 
            testval = float(val)
        except: 
            self.nbadvals += 1
            return 
        self.nvals += 1
        self.minval = val if val < self.minval else self.minval
        self.maxval = val if val > self.maxval else self.maxval
        if val != math.floor( val ):  
            self.allint = False
        if val < 0.0:  
            self.allpos = False
        elif val > 0.0:  
            self.allneg = False
        return 

    def result(self, nearest):
        """ return result dict.  """
        if not self.active:
            raise RuntimeError(f'findrange: no result, never activated')
        if self.nvals == 0:
            self.active = False
            raise RuntimeError(f'findrange: no result, no usable data presented, '
                               f' number of invalid values={self.nbadvals}')

        # determine inc for 'nearest' purposes..
        inc = nearest
        if not inc: 
            inc = getinc(self.minval, self.maxval)
            if inc < 1.0 and self.allint: 
                 inc = 1.0

        # back off the min (and advance the max) to nearest "whole" increment...
        h = math.fmod( self.minval, inc )
        if h == 0.0:  
            h = inc # min is on the boundary; add an extra inc
        if self.minval < 0.0: 
            axmin = self.minval - (inc+h)    # must go the other way when negative  
        else: 
            axmin = self.minval - h          # anything else needed here?

        h = inc - math.fmod(self.maxval, inc)
        if h == 0.0:  
            h = inc                     # max is on the boundary; add an extra inc
        else: 
            axmax = self.maxval + h

        # if addlpad: 
        #     axmin -= (inc*addlpad)
        #     axmax += (inc*addlpad)

        # guard against unnecessary zero-crossings...  
        # (should this be controllable?) TODO
        axmin = 0.0 if self.allpos and axmin < 0.0 else axmin
        axmax = 0.0 if self.allneg and axmax > 0.0 else axmax

        if self.allint:
            axmin = int(axmin)
            axmax = int(axmax)
            self.minval = int(self.minval)
            self.maxval = int(self.maxval)

        self.active = False 

        return {'axmin':axmin, 'axmax':axmax, 'datamin':self.minval, 'datamax':self.maxval, 
                'inc':inc, 'nvals':self.nvals, 'nbadvals':self.nbadvals,
                'allint':self.allint, 'allpos':self.allpos, 'allneg':self.allneg}


def _catinfo(datarows, column, accumcol, nulls):
    """ return a dict of observed category terms (in the order encountered) with 
        frequency counts (or accums).  
        The datarows, column, accumcol args work as described for _numinfo() below.
        The nulls arg controls how None is handled (ignore or keep as spacer).
    """
    catdict = {}
    nullscount = 0
    for row in datarows:
        if column == None:
            strval = row
        else:
            try:
                strval = row[column]
            except:
                strval = None
        tallyval = 1
        if accumcol:
            try:
                tallyval = row[accumcol]  # dict or list rows 
            except:
                pass
        strval = '' if not strval else strval
        if strval == '' and nulls == 'ignore':
            continue
        elif strval == '' and nulls == 'spacers':
            nullscount += 1
            strval = f'!spacer:{nullscount}' # sentry val (axis)
        try:    
            x = catdict[strval]
            catdict[strval] += tallyval
        except:
            catdict[strval] = tallyval
    return catdict 


def _numinfo(datarows, column, find_distrib, reqbinsize, accumcol, find_percentiles):
    """ Find characteristics of a numeric data column/variable.
        :param datarows: a list of tuple or list of dict to iterate over
        :param column:   selects desired item from each row... 
                          int for tuples or element name for dicts
                          or None (means datarows are just a 1-D list)
        (if no useful values encountered, return None).
    """
    nvals = nbadvals = 0
    sum = sumsq = 0.0
    allint = allpos = allneg = numsorted = True; 
    minval = 9.99e+99; maxval = -9.99e+99; prevval = maxval; 
    icount = 0
    errcount = 0
    for row in datarows:
        icount += 1
        if column == None:
            strval = row 
        else:
            try:
                strval = row[column]
            except:
                print(f'columninfo: failed to get column {column} from row {icount}')
                strval = None   # will be rejected below
        try: 
            fval = float(strval)
        except: 
            nbadvals += 1
            continue
        nvals += 1
        minval = fval if fval < minval else minval
        maxval = fval if fval > maxval else maxval
        if fval != math.floor( fval ):  
            allint = False
        if fval < 0.0:  
            allpos = False
        elif fval > 0.0:  
            allneg = False
        if fval < prevval: 
            numsorted = False
        sum += fval
        sumsq += (fval*fval)
 
    if nvals == 0: return None

    mean = sum / float(nvals)
    sd = math.sqrt( ( sumsq - ((sum*sum)/float(nvals)) ) / float(nvals-1) )
    sem = sd / math.sqrt( float(nvals) )

    binsize = None
    distbins = []
    if find_distrib:   # run a freq distribution
        # compute inc and axmax, axmin ... needed for calculating distribution
        inc = getinc(minval, maxval)
        if inc < 1.0 and allint == True: 
            inc = 1.0

        # back off the min (and advance the max) to nearest "whole" increment...
        h = math.fmod(minval, inc)
        if h == 0.0:  
            h = inc # min is on the boundary; add an extra inc
        axmin = minval - h
        h = inc - math.fmod( maxval, inc )
        if h == 0.0:  h = inc # max is on the boundary; add an extra inc
        axmax = maxval + h

        # guard against flukes...
        if allpos == True and axmin < 0.0:  axmin = 0.0
        if allneg == True and axmax > 0.0:  axmax = 0.0

        try:
            if not reqbinsize: 
                binsize = inc/2.0
            elif str(reqbinsize)[:4] == "inc/": 
                binsize = inc / math.fabs(float(reqbinsize[4:]))
            else: 
                binsize = math.fabs(float(reqbinsize))
        except: 
            raise RuntimeError(f'numinfo: failed to find distribution binsize')
        print(f'numinfo: notice, freq distibution binsize is: {binsize}')
        halfbin = binsize / 2.0
        curval = axmin
        while curval < axmax:
            distbins.append({'binmid':curval+halfbin, 'binlo':curval, 'accum':0})
            curval += binsize
        for row in datarows:
            if column == None:
                strval = row
            else:
                try:
                    strval = row[column]
                except:
                    strval = None   # will be rejected below
            try: 
                fval = float(strval)
            except: 
                continue

            tallyval = 1
            if accumcol != None:
                try:
                    tallyval = row[accumcol]  
                except:
                    pass

            for bin in distbins:   # put in a bin
                if fval < (bin['binmid']+halfbin): 
                    bin['accum'] += tallyval
                    break
    pcl = None
    if find_percentiles:
        pcl = compute_percentiles(datarows, column)

    return {'min':minval, 'max':maxval, 'nvals':nvals, 'nbadvals':nbadvals, 'allint':allint,
            'numsorted':numsorted, 'mean':mean, 'sd':sd, 'sem':sem, 'sum':sum, 
            'distribution':distbins, 'distbinsize':binsize, 'percentiles':pcl}


def compute_percentiles(datarows, column):
    """ compute 5th, 25th, 50th, 75th, and 95th percentiles on a data column.
        Make a vector of values, leaving out any non-numerics, sort it, compute pctiles.
    """
    nums= []
    for row in datarows:
        if column == None:
            strval = row
        else:
            try:
                strval = row[column]
            except:
                continue
        try:
            fval = float(strval)
        except:
            continue
        nums.append(fval)
    nvals = len( nums)
    if nvals < 3:
        print(f'columninfo: not enough values to compute percentiles.')
        return None

    nums = sorted(nums)

    cell = nvals//20; 
    p5  = nums[cell] if nvals % 20 != 0 else (nums[cell-1] + nums[cell])/2.0

    cell = nvals//4; 
    p25 = nums[cell] if nvals %  4 != 0 else (nums[cell-1] + nums[cell])/2.0

    cell = nvals//2; 
    p50 = nums[cell] if nvals %  2 != 0 else (nums[cell-1] + nums[cell])/2.0

    cell = (nvals-(nvals//4))-1; 
    p75 = nums[cell] if nvals %  4 != 0 else (nums[cell] + nums[cell+1])/2.0

    cell = (nvals-(nvals//20))-1; 
    p95 = nums[cell] if nvals % 20 != 0 else (nums[cell] + nums[cell+1])/2.0

    return {'p5':p5, 'p25':p25, 'median':p50, 'p75':p75, 'p95':p95}


def polar(cx, cy, direction, magnitude):
    """ given polar coords, return native units """
    theta = (direction / 360.0) * (4.0*halfpi)
    x = cx + (magnitude * math.cos(theta));
    y = cy + (magnitude * math.sin(theta))
    return x, y


def arrowhead(x1, y1, x2, y2, headlen, headwid):
    """ given tip (x,y) etc, return coords of 2 other pts in triangle """
    vx = x2 - x1;
    vy = y2 - y1;
    if vx == 0.0 and y2 > y1:
        th0 = halfpi      # avoid divide by zero
    elif vx == 0.0 and y1 > y2:
        th0 = -(halfpi)   # avoid divide by zero
    else:
        th0 = math.atan(vy/vx)
    th1 = th0 + headwid;
    th2 = th0 - headwid;
    r = headlen
    if x2 < x1:
        return x2+(r*math.cos(th1)), y2+(r*math.sin(th1)), \
               x2+(r*math.cos(th2)), y2+(r*math.sin(th2))
    else:
        return x2-(r*math.cos(th1)), y2-(r*math.sin(th1)), \
               x2-(r*math.cos(th2)), y2-(r*math.sin(th2))


def dpcluster(clust, x, y):
    """ return small cx,cy offset for duplicate datapoints, or if 
        clust['mode'] = 'omit_dups' inform caller that duplicate datapoint need not be rendered.
        Data must be presented in sorted order on x, y .
        Returns 3 items: newx, newy, and omit (either True or False)
    """
    cx = cy = 0.0
    # cluster if (x,y) is same location (or nearly) as previous... must use fabs() here
    if math.fabs(x-clust['prevx']) <= clust['tol'] and \
       math.fabs(y-clust['prevy']) <= clust['tol']:        
        mode = clust['mode']
        clust['ndup'] += 1
        if mode == 'omit_dups':
            return 0.0, 0.0, True
        ndup = int(clust['ndup']/clust['dampen'])
        ofs = clust['offset']
        nlist = len(clust_xofs)
        if mode == 'surround':
            cx = clust_xofs[ndup % nlist] * ofs
            cy = clust_yofs[ndup % nlist] * ofs
        elif mode == 'upward':
            cy = ndup * ofs * 4.0
        elif mode == 'left+right':
            cx = int(ndup/2.0) * ofs 
            cx = -cx if ndup % 2 == 0 else cx
        elif mode == 'rightward':
            cx = ndup * ofs * 4.0
        elif mode == 'leftward':
            cx = ndup * ofs * -4.0
        elif mode == 'downward':
            cy = ndup * ofs * -4.0
        elif mode == 'up+down':
            cy = int(ndup/2.0) * ofs 
            cy = -cy if ndup % 2 == 0 else cy
        if clust['conform']:   # avoid 'teetering'
            if mode in ['upward', 'downward', 'up+down']:
                cx = clust['prevx'] - x
            elif mode in ['leftward', 'rightward', 'left+right']:
                cy = clust['prevy'] - y
        if clust['jitter'] != 0.0:
            cx += (random.random() - 0.5) * clust['jitter']
            cy += (random.random() - 0.5) * clust['jitter']
    else:
        clust['prevx'] = x 
        clust['prevy'] = y
        clust['ndup'] = 0
    return cx, cy, False

