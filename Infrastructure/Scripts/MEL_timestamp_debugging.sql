 SELECT a.*
 FROM (select ### site login ###
 				u.*,
                u.last_logged_in as 'timestamp',
                "site_login" as 'action',
                "4" as 'action_id',
                null as 'source',
                "0" as 'action_serial_id'
              from
                quasar.users_log u
			  where u.last_logged_in > '0000-00-00 00:00:00'
			  ORDER BY u.northstar_created_at_timestamp DESC, u.northstar_id, u.last_logged_in) a
 WHERE a.timestamp <= '1970-01-01 00:00:01' AND a.timestamp IS NOT NULL 
 LIMIT 5000;
 #How many of these are niche
 #How many people text in without signing up for a campaign?
 #How many people register and have nothing else?
 
 SELECT * FROM quasar.users_log l WHERE l.northstar_id='59826fd8a0bfad01f90c7f71' ORDER BY l.last_logged_in;
 SELECT * FROM quasar.users u WHERE u.northstar_id='59826fd8a0bfad01f90c7f71';

 SELECT a.*
 FROM 
   (select ### site login ###
                u.northstar_id as 'northstar_id',
                u.last_logged_in as 'timestamp',
                "site_login" as 'action',
                "4" as 'action_id',
                null as 'source',
                "0" as 'action_serial_id'
              from
                quasar.users_log u
              where u.last_logged_in > '0000-00-00 00:00:00') a
 WHERE a.timestamp <= '1970-01-01 00:00:01'
 LIMIT 500