DROP TABLE IF EXISTS current_site_info;
DROP TABLE IF EXISTS new_site_info;

CREATE TEMPORARY TABLE current_site_info AS 
    (SELECT
        MD5(concat(a.northstar_id, a.timestamp, a.action_id, a.action_serial_id)) AS event_id,
        a.northstar_id AS northstar_id,
        a.timestamp AS timestamp,
        a.action AS action_type,
        a.action_id AS action_id,
        a.source AS source,
        a.action_serial_id AS action_serial_id
    FROM ( 
        SELECT DISTINCT 
            u_access.northstar_id,
            u_access.timestamp,
            'site_access' AS action,
            '3' AS action_id,
            NULL AS source,
            '0' AS action_serial_id
        FROM 
            (SELECT -- SITE ACCESS
                DISTINCT u_new_acc.id AS northstar_id,
                u_new_acc.last_accessed_at AS timestamp
            FROM
                northstar.users u_new_acc
            WHERE u_new_acc.last_accessed_at IS NOT NULL 
            UNION ALL 
            SELECT --SITE ACCESS LEGACY
                DISTINCT u_leg_acc.northstar_id,
                u_leg_acc.last_accessed AS timestamp
            FROM 
                northstar.users_log_mysql u_leg_acc
            WHERE u_leg_acc.last_accessed IS NOT NULL) u_access
        UNION ALL 
        SELECT DISTINCT 
            u_login.northstar_id,
            u_login.timestamp,
            'site_login' AS action,
            '4' AS action_id,
            NULL AS source,
            '0' AS action_serial_id
        FROM 
            (SELECT -- SITE LOGIN
                DISTINCT u_new_login.id AS northstar_id,
                u_new_login.last_authenticated_at AS timestamp
            FROM
                northstar.users u_new_login
            WHERE u_new_login.last_authenticated_at IS NOT NULL 
            UNION ALL 
            SELECT --SITE LOGIN LEGACY
                DISTINCT u_leg_login.northstar_id,
                u_leg_login.last_logged_in AS timestamp
            FROM 
                northstar.users_log_mysql u_leg_login
            WHERE u_leg_login.last_logged_in IS NOT NULL) u_login
                    ) AS a 
        ); 
        
;


CREATE TEMPORARY TABLE new_site_info AS 
    (SELECT
        MD5(concat(a.northstar_id, a.timestamp, a.action_id, a.action_serial_id)) AS event_id,
        a.northstar_id AS northstar_id,
        a.timestamp AS timestamp,
        a.action AS action_type,
        a.action_id AS action_id,
        a.source AS source,
        a.action_serial_id AS action_serial_id
    FROM ( 
        SELECT DISTINCT 
            u_access.northstar_id,
            u_access.timestamp,
            'site_access' AS action,
            '3' AS action_id,
            NULL AS source,
            '0' AS action_serial_id
        FROM 
            (SELECT -- SITE ACCESS
                DISTINCT u_new_acc.id AS northstar_id,
                u_new_acc.last_accessed_at AS timestamp
            FROM
                northstar.users u_new_acc
            WHERE u_new_acc.last_accessed_at IS NOT NULL 
            UNION ALL 
            SELECT --SITE ACCESS LEGACY
                DISTINCT u_leg_acc.id AS northstar_id,
                u_leg_acc.last_accessed_at AS timestamp
            FROM 
                public.users_log_to_users_test u_leg_acc
            WHERE u_leg_acc.last_accessed_at IS NOT NULL) u_access
        UNION ALL 
        SELECT DISTINCT 
            u_login.northstar_id,
            u_login.timestamp,
            'site_login' AS action,
            '4' AS action_id,
            NULL AS source,
            '0' AS action_serial_id
        FROM 
            (SELECT -- SITE LOGIN
                DISTINCT u_new_login.id AS northstar_id,
                u_new_login.last_authenticated_at AS timestamp
            FROM
                northstar.users u_new_login
            WHERE u_new_login.last_authenticated_at IS NOT NULL 
            UNION ALL 
            SELECT --SITE LOGIN LEGACY
                DISTINCT u_leg_login.id AS northstar_id,
                u_leg_login.last_authenticated_at AS timestamp
            FROM 
                public.users_log_to_users_test u_leg_login
            WHERE u_leg_login.last_authenticated_at IS NOT NULL) u_login
                    ) AS a 
        ); 
        
SELECT substring("timestamp"::varchar, 1, 7), action_type, count(*) FROM new_site_info GROUP BY substring("timestamp"::varchar, 1, 7), action_type ORDER BY substring("timestamp"::varchar, 1, 7), action_type;
SELECT substring("timestamp"::varchar, 1, 7), action_type, count(*) FROM current_site_info GROUP BY substring("timestamp"::varchar, 1, 7), action_type ORDER BY substring("timestamp"::varchar, 1, 7), action_type;