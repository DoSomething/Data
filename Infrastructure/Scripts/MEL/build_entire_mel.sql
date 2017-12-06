DROP TEMPORARY TABLE IF EXISTS temp_mel;
CREATE TEMPORARY TABLE IF NOT EXISTS sms_game_log AS 
(select ###mobile campaign historic data
			         concat(if(mml.mu_uid > 0 , mml.mu_uid, mml.ms_phone_number), (mml.ms_activated_at), mml.mc_run_id, 6) as 'event_id',
			         if(mml.mu_uid > 0 , mml.mu_uid, mml.ms_phone_number) as 'uid',
			          (mml.ms_activated_at) as 'timestamp',
			          "sms_game" as 'action',
			          "6" as 'action_id',
			          mml.mc_run_id as 'run_nid',
			          mml.n_title as 'run_name'
			      from users_and_activities.mobile_master_lookup_lite mml
			      left join quasar.campaign_info ci
			        on ci.campaign_run_id = mml.mc_run_id
			      where (ci.campaign_type = 'sms_game'
			      OR ci.campaign_run_id = 7931))
			      ;
CREATE TEMPORARY TABLE IF NOT EXISTS temp_mel AS 
(SELECT
              concat(a.northstar_id, a.timestamp, a.action_id, a.action_serial_id) as 'event_id',
              a.northstar_id as 'northstar_id',
              a.timestamp as 'timestamp',
              a.action as 'action_type',
              a.action_id as 'action_id',
              a.source as 'source'
            from(
              select ### campaign sign up ###
                ca.northstar_id as 'northstar_id',
                ca.signup_created_at as 'timestamp',
                "sign-up" as 'action',
                "1" as 'action_id',
                ca.signup_source as 'source',
                ca.signup_id as 'action_serial_id'
              from
                quasar.campaign_activity ca
              where ca.signup_created_at > '0000-00-00 00:00:00'
            union all
              select ### campaign reportback ###
                rb.northstar_id as 'northstar_id',
                rb.submission_created_at as 'timestamp',
                "reportback" as 'action',
                "2" as 'action_id',
                rb.post_source as 'source',
                rb.post_id as 'action_serial_id'
              from
                quasar.campaign_activity rb
                where rb.post_id > 0
            union all
              select ### site access ###
                u.northstar_id as 'northstar_id',
                u.last_accessed as 'timestamp',
                "site_access" as 'action',
                "3" as 'action_id',
                null as 'source',
                "0" as 'action_serial_id'
              from
                quasar.users_log u
              where u.last_accessed >= u.northstar_created_at_timestamp
            union all
              select ### site login ###
                u.northstar_id as 'northstar_id',
                u.last_logged_in as 'timestamp',
                "site_login" as 'action',
                "4" as 'action_id',
                null as 'source',
                "0" as 'action_serial_id'
              from
                quasar.users_log u
              where u.last_logged_in >= u.northstar_created_at_timestamp
             union all
              select ### sms game log ###
                qu.northstar_id as 'northstar_id',
                sgl.timestamp as 'timestamp',
                sgl.action as 'action',
                sgl.action_id as 'action_id',
                null as 'source',
                "0" as 'action_serial_id'
              from
                sms_game_log sgl
              left join quasar.users qu
                on qu.drupal_uid = sgl.uid) as a);
              
  SELECT m.action_type, count(*) FROM temp_mel m GROUP BY m.action_type;
  SELECT * FROM temp_mel LIMIT 100;
  
  SELECT * FROM campaign_info i WHERE i.campaign_run_id=7931 