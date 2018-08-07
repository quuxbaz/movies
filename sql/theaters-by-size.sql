select count(distinct showtimes.mov_rootId) as N, theaters.* from showtimes 
  join theaters on theaters.th_id = showtimes.th_id
group by showtimes.th_id order by N desc