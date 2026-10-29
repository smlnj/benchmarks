
import datetime as d
import time
import calendar    

# non-mutable globals

nsec = {'day':86400, '12hour':43200, '6hour':21600, '4hour':14400, '3hour':10800, 
        '2hour':7200, 'hour':3600, '30minute':1800, '10minute':600, 'minute':60}


def _intdt(datestr, dtformat):
    """ convert a date/time string to int utime value... '1970-01-01.00:00' --> 0 """
    if not datestr:
        return None
    try:
        tt = d.datetime.strptime(datestr, dtformat).timetuple() # parse the components
        return calendar.timegm(tt)    # return utime
    except Exception as errmsg: 
        raise ValueError(f"intdt: '{datestr}' parse failed using format '{dtformat}'")


def _outdt(utime, dtformat):
    """ format the given int utime value as a date/time string """
    if not utime:
        return None
    try:
        return d.datetime.utcfromtimestamp(utime).strftime(dtformat)
    except: 
        raise ValueError(f'outdt: failed on utime={utime}')


def _dtdiff(dt1, dt2, outunits, dtformat):
    """ return difference dt1 - dt2 in days or other unit """
    try: 
        val1 = d.datetime.strptime(dt1, dtformat)
        val2 = d.datetime.strptime(dt2, dtformat)
    except: 
        raise ValueError(f"datediff: bad arg '{dt1}' or '{dt2}' (expected format is '{dtformat}')")
    if outunits != 'seconds':
        val1 = val1.replace(second=0, microsecond=0)
        val2 = val2.replace(second=0, microsecond=0)
    if outunits == 'days': 
        val1 = val1.replace(hour=0, minute=0)
        val2 = val2.replace(hour=0, minute=0)
        div = 86400
    elif outunits == 'hours': 
        val1 = val1.replace(minute=0)
        val2 = val2.replace(minute=0)
        div = 3600
    elif outunits == 'minutes': 
        div = 60
    elif outunits == 'seconds': 
        div = 1
    return int(calendar.timegm(val1.timetuple()) - calendar.timegm(val2.timetuple())) / div


def _finalize_dt_range(rangedict, nearest, weekday0):    
    """ do axis range finalization. (Defaults set in sds.py)
        incoming rangedict is the findrange result from utime values, and it contains:
           {axmin, axmax, datamin, datamax, nvals, nbadvals, allint, allpos, allneg}
        TODO... do we need nearest='exact' ?
    """
    # int utime values supplied by findrange
    umin = rangedict['datamin']   
    umax = rangedict['datamax']

    # always zero out seconds and ms...
    dtmin = d.datetime.utcfromtimestamp(umin).replace(second=0, microsecond=0)   
    dtmax = d.datetime.utcfromtimestamp(umax).replace(second=0, microsecond=0)
    if nearest[-6:] != "minute": 
        dtmin = dtmin.replace(minute=0) # zero out minutes usually
        dtmax = dtmax.replace(minute=0)   

    # retain the raw min and max in dt format...
    datamin = dtmin
    datamax = dtmax

    # find adjusted dtmin and dtmax...
    # dtmin will be the start of an interval; 
    # dtmax will be the start of the following interval (axis drawing)
    if nearest == 'year':
        dtmin = dtmin.replace(month=1, day=1, hour=0)
        yr = dtmax.year;
        dtmax = dtmax.replace(year=yr+1, month=1, day=1, hour=0)
    elif nearest == '3month':
        qmin = (((dtmin.month-1) // 3) *3) +1    # quarter year start
        dtmin = dtmin.replace(month=qmin, day=1, hour=0)
        qmax = ((((dtmax.month-1) // 3)+1) *3)
        qmax += 1    # advance to the start of the next quarter year (axis drawing)
        yr = dtmax.year;
        if qmax > 12:
            qmax = 1
            yr += 1
        dtmax = dtmax.replace(year=yr, month=qmax, day=1, hour=0)
        # dtmax = dtmax.replace(year=yr, month=newmon, day=1, hour=0)
    elif nearest == 'month':   
        dtmin = dtmin.replace(day=1, hour=0)
        mon = dtmax.month; 
        yr = dtmax.year;
        if mon == 12: 
            dtmax = dtmax.replace(year=yr+1, month=1, day=1, hour=0)
        else:         
            dtmax = dtmax.replace(month=mon+1, day=1, hour=0)
    elif nearest == 'week':  
        # (struct_time tm_wday convention is that 0 = monday)
        wday = time.gmtime(umin).tm_wday   
        wday += (7-weekday0)%7   # adjust for user-preferred week boundary...
        # deduct no. of days needed to reach opening week boundary (86400 sec per day)...
        umin -= (wday*86400)                 
        dtmin = d.datetime.utcfromtimestamp(umin).replace(hour=0)
        wday = 7 - time.gmtime(umax).tm_wday
        wday += (7-weekday0)%7   # adjust for user-preferred week boundary...
        # add no. of days needed to reach the next week boundary...
        umax += (wday*86400)                 
        dtmax = d.datetime.utcfromtimestamp(umax).replace(hour=0)
    elif nearest == 'day':
        dtmin = dtmin.replace(hour=0)
        umax += 86400  # jump forward one day
        dtmax = d.datetime.utcfromtimestamp(umax).replace(hour=0)
    elif nearest in ['12hour', '6hour', '4hour', '3hour']:
        nhr = int(nearest[:-4])
        newhr = int((dtmin.hour // nhr) * nhr)
        dtmin = dtmin.replace(hour=newhr)
        newhr = int(((dtmax.hour // nhr)+1) * nhr)
        day = dtmax.day
        if newhr >= 24: 
            newhr = 0
            day += 1
        dtmax = dtmax.replace(day=day, hour=newhr)
    elif nearest == 'hour':
        dtmin = dtmin.replace(minute=0)
        hr = dtmax.hour
        if hr == 23:      
            umax += 3600  # jump forward one hour (3600 sec per hour)
            dtmax = d.datetime.utcfromtimestamp(umax)   # no replace necessary
        else: 
            dtmax = dtmax.replace(hour=hr+1, minute=0)
    elif nearest in ['30minute', '10minute']:
        nmin = int(nearest[:-6])
        newmin = int((dtmin.minute // nmin) * nmin)
        dtmin = dtmin.replace(minute=newmin)
        newmin = int(((dtmax.minute // nmin)+1) * nmin)
        hr = dtmax.hour
        if newmin >= 60: 
            newmin = 0; 
            hr += 1    # (date rollover not imp.)
        dtmax = dtmax.replace(hour=hr, minute=newmin)
    elif nearest == 'minute':
        # dtmin is all set, just compute dtmax...
        newmin = dtmax.minute + 1
        hr = dtmax.hour
        if newmin >= 60: 
            newmin = 0
            hr += 1
        dtmax = dtmax.replace( hour=hr, minute=newmin )
    else: 
        raise ValueError(f"findrange_dtresult: unrecognized nearest='{nearest}'")

    axmin = calendar.timegm( dtmin.timetuple() )
    axmax = calendar.timegm( dtmax.timetuple() )

    rangedict['axmin'] = axmin  # utime
    rangedict['axmax'] = axmax  # utime
    rangedict.update({'dt_axmin':dtmin, 'dt_axmax':dtmax, 'dt_inc':nearest,
                         'dt_datamin':datamin, 'dt_datamax':datamax})
    return rangedict


def _datestubs(rangedict, inc, crossings, dtformat, terse, weekday0, fymonth1): 
    """ return a list of ready-to-render stubs with int utime positions.
        crossings=True: only make stubs when crossing an inc boundary
    """
    try:
        testval = rangedict['dt_inc']
    except:
        raise ValueError(f'datestubs: rangedict has no dt info')
    axmin = rangedict['axmin']
    axmax = rangedict['axmax']
    dtmin = rangedict['dt_axmin']
    dtmax = rangedict['dt_axmax']
    dtformat = '' if not dtformat else dtformat  
    firststub = None
    cross_inc = None
    stublist = []

    # first stub
    stub = tersen(dtmin.strftime(dtformat), dtformat, terse)

    if crossings:
        cross_inc = inc
        inc = rangedict['dt_inc']
        if cross_inc not in ['year', 'fy', '3month', 'month', 'week', 'day']:
            raise ValueError(f'datestubs: bad inc with crossings=True: {cross_inc}')

    if cross_inc == 'fy':
        yy = int(str(dtmin.year)[-2:])
        stub = f"FY'{yy+1}" if dtmin.month != 1 else f"FY'{yy}"  # rewrite

    if crossings:
        firststub = (axmin, stub)    # remember this in case needed
    else:
        stublist.append((axmin, stub))

    dtcur = dtmin
    utime = axmin
    iloop = 0
    while iloop < 500:   # 500 = sanity backstop
        yr = dtcur.year
        mon = dtcur.month
        day = dtcur.day
        if inc == 'year':
            dtcur = dtcur.replace(year=yr+1)
        elif inc == 'month':
            if mon == 12: 
                dtcur = dtcur.replace(year=yr+1, month=1)
            else: 
                dtcur = dtcur.replace(month=mon+1)
        elif inc == '3month':
            if mon >= 10: 
                dtcur = dtcur.replace(year=yr+1, month=1)
            else: 
                dtcur = dtcur.replace(month=mon+3)
        elif inc == 'week':
            utime += 604800       # number of seconds in a 7 day week
        else:
            try:
               utime += nsec[inc]
            except KeyError:
               raise ValueError(f"datestubs: unrecognized inc='{inc}'")

        if inc not in ['year', 'month', '3month']: 
            dtcur = d.datetime.utcfromtimestamp( utime )
        if inc != 'day': 
            utime = calendar.timegm( dtcur.timetuple() ) 

        stub = tersen(dtcur.strftime(dtformat), dtformat, terse)

        if utime > axmax: 
            break


        if crossings and \
            ((cross_inc == 'year' and dtcur.year != yr) or  \
             (cross_inc in ['month','3month'] and dtcur.month != mon) or  \
             (cross_inc == 'fy' and dtcur.month == fymonth1) or  \
             (cross_inc == 'week' and time.gmtime(utime).tm_wday == weekday0) or \
             (cross_inc == 'day' and dtcur.day != day)):

            if cross_inc == 'fy':
                yy = int(str(yr)[-2:])
                stub = f"FY'{yy+1}" if mon != 1 else f"FY'{yy}"
            stublist.append((utime, stub))  # do this first

            if firststub:  #  and cross_inc in ['year', 'fy', 'month', 'week', 'day']:
                stublist.append(firststub)  # do 2nd, so stubcull removes it if crowded
            firststub = None
        elif not crossings:
            stublist.append((utime, stub))  

        iloop += 1

    if firststub:    # in case first stub is the only stub...
        stublist.append(firststub)  

    if len(stublist) == 0:
        if crossings:
            raise RuntimeError('datestubs: empty result, unsupported crossings use-case?')
        raise RuntimeError('datestubs: empty result, no stubs were generated')

    return stublist


def tersen(val, dtformat, terse):
    """ return terse version of a stub """
    if not terse or not val:
        return val
    elif dtformat[:2] in ['%a', '%b']:
        result = val[:1]+val[3:]
    elif dtformat[:2] in ['%m', '%d', '%H', '%I', '%M']:    # strip off leading zero
        result = val[1:] if val[0] == '0' else val
    else:
        result = val
    result = result.replace('AM', 'a').replace('PM', 'p')
    return result
