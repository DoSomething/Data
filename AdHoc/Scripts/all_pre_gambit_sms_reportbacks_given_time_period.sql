SELECT *
FROM dosomething.dosomething_reportback rb
LEFT JOIN dosomething.dosomething_signup ds
    ON rb.uid = ds.uid 
    AND rb.run_nid = ds.run_nid
WHERE (FROM_UNIXTIME(rb.created)) >= (TIMESTAMP('2016-08-01')) 
AND (FROM_UNIXTIME(rb.created)) < (TIMESTAMP('2016-09-01'))
AND ds.sid IS NULL