# Definition for an interval.
# class Interval:
#     def __init__(self, s=0, e=0):
#         self.start = s
#         self.end = e

class Solution:
    # @param intervals, a list of Intervals
    # @param new_interval, a Interval
    # @return a list of Interval
    def insert(self, intervals, new_interval):
        if not intervals:
            return [new_interval]
            
        s = intervals[0].start
        e = intervals[0].end
        res = []
        
        if new_interval.end < s:
            res.append(new_interval)
        
        inserted = False
        for i in range(len(intervals)):
            ns = intervals[i].start
            ne = intervals[i].end
            
            if (ns <= new_interval.end and new_interval.start <= ne) and not inserted:
                # start of insert, a merge
                if i:
                    res.append(Interval(s, e))
                    
                s = min(ns, new_interval.start)
                e = max(ne, new_interval.end)
                inserted = True
            elif e >= intervals[i].start:
                # a merge, or first iter
                e = max(e, intervals[i].end)
            else:
                #nothing merge-y
                res.append(Interval(s, e))
                
                if e < new_interval.start and new_interval.end < ns:
                    res.append(new_interval)
                    
                s = ns
                e = ne
        
        res.append(Interval(s, e))
        if new_interval.start > intervals[-1].end:
            res.append(new_interval)
            
        return res
                