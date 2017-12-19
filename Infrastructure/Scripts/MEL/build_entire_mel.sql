DROP TABLE IF EXISTS quasar.member_event_log;
DROP TABLE IF EXISTS quasar.sms_game_log;

CREATE TABLE IF NOT EXISTS sms_game_log AS 
	(SELECT 
		concat(IF(mml.mu_uid > 0 , mml.mu_uid, mml.ms_phone_number), (mml.ms_activated_at), mml.mc_run_id, 6) AS 'event_id',
		IF(mml.mu_uid > 0 , mml.mu_uid, mml.ms_phone_number) AS 'uid',
		(mml.ms_activated_at) AS 'timestamp',
		"sms_game" AS 'action',
		"6" AS 'action_id',
		mml.mc_run_id AS 'run_nid',
		mml.n_title AS 'run_name'
	FROM users_and_activities.mobile_master_lookup_lite mml
	LEFT JOIN quasar.campaign_info ci
		ON ci.campaign_run_id = mml.mc_run_id
	WHERE (ci.campaign_type = 'sms_game' OR ci.campaign_run_id = 7931));
	
CREATE TABLE IF NOT EXISTS temp_mel AS 
	(SELECT
    		concat(a.northstar_id, a.timestamp, a.action_id, a.action_serial_id) AS 'event_id',
        a.northstar_id AS 'northstar_id',
        a.timestamp AS 'timestamp',
        a.action AS 'action_type',
        a.action_id AS 'action_id',
        a.source AS 'source'
	FROM 
		(SELECT ### campaign sign up ###
			ca.northstar_id AS 'northstar_id',
            	ca.signup_created_at AS 'timestamp',
            	"sign-up" AS 'action',
            	"1" AS 'action_id',
            	ca.signup_source AS 'source',
            	ca.signup_id AS 'action_serial_id'
        	FROM quasar.campaign_activity ca
        	WHERE ca.signup_created_at > '0000-00-00 00:00:00'
        	UNION ALL
        	SELECT ### campaign reportback ###
			rb.northstar_id AS 'northstar_id',
            rb.submission_created_at AS 'timestamp',
            "reportback" AS 'action',
            "2" AS 'action_id',
            rb.post_source AS 'source',
            rb.post_id AS 'action_serial_id'
        FROM quasar.campaign_activity rb
		WHERE rb.post_id > 0
		UNION ALL
		SELECT ### site access ###
        		u.northstar_id AS 'northstar_id',
            u.last_accessed AS 'timestamp',
            "site_access" AS 'action',
            "3" AS 'action_id',
            NULL AS 'source',
            "0" AS 'action_serial_id'
        FROM quasar.users_log u
        WHERE u.last_accessed >= u.northstar_created_at_timestamp
        UNION ALL
        SELECT ### site login ###
        		u.northstar_id AS 'northstar_id',
            u.last_logged_in AS 'timestamp',
            "site_login" AS 'action',
            "4" AS 'action_id',
            NULL AS 'source',
            "0" AS 'action_serial_id'
        FROM quasar.users_log u
        WHERE u.last_logged_in >= u.northstar_created_at_timestamp
        UNION ALL
       	SELECT ### sms game log ###
        	qu.northstar_id AS 'northstar_id',
            sgl.timestamp AS 'timestamp',
            sgl.action AS 'action',
            sgl.action_id AS 'action_id',
            NULL AS 'source',
            "0" AS 'action_serial_id'
        FROM sms_game_log sgl
        LEFT JOIN quasar.users qu on qu.drupal_uid = sgl.uid) AS a);