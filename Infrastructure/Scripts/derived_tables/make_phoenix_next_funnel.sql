DROP MATERIALIZED VIEW IF EXISTS phoenix_next_funnel;
CREATE MATERIALIZED VIEW IF NOT EXISTS phoenix_next_funnel AS (
SELECT
      step1.device_id AS device_id,
      step1.created_at AS step1,
      step2.created_at AS step2,
      step3.created_at AS step3,
      step4.created_at AS step4,
      step1.campaign_id AS campaign_id,
      step1.href AS href,
      step1.referrer_href AS referrer_href,
      step1.page_utm_campaign AS utm_campaign
      FROM
        (SELECT ps.device_id, pe.campaign_id, pe.href, ps.referrer_href, pe.page_utm_campaign, min((to_timestamp(pe.ts / 1000))) AS created_at
         FROM public.phoenix_next_events pe
         LEFT JOIN public.phoenix_next_sessions ps
            ON pe.session_id = ps.session_id
         WHERE pe.event_name = 'visit'
         GROUP BY ps.device_id, pe.campaign_id, pe.href, ps.referrer_href, pe.page_utm_campaign) AS step1
       LEFT JOIN
        (SELECT ps.device_id, min((to_timestamp(pe.ts / 1000))) AS created_at
         FROM public.phoenix_next_events pe
         LEFT JOIN public.phoenix_next_sessions ps
            ON pe.session_id = ps.session_id
         WHERE pe.event_name = 'signup'
         GROUP BY ps.device_id)  AS step2
       ON step2.device_id = step1.device_id
       LEFT JOIN
        (SELECT ps.device_id, min((to_timestamp(pe.ts / 1000))) AS created_at
         FROM public.phoenix_next_events pe
         LEFT JOIN public.phoenix_next_sessions ps
            ON pe.session_id = ps.session_id
         WHERE pe.event_name = 'view'
         AND pe.northstarid_s IS NOT NULL
         GROUP BY ps.device_id)  AS step3
       ON step3.device_id = step2.device_id
       LEFT JOIN
        (SELECT ps.device_id, min((to_timestamp(pe.ts / 1000))) AS created_at
         FROM public.phoenix_next_events pe
         LEFT JOIN public.phoenix_next_sessions ps
            ON pe.session_id = ps.session_id
         WHERE (pe.event_name = 'Successful Reportback' OR pe.event_name = 'share action completed')
         GROUP BY ps.device_id)  AS step4
       ON step4.device_id = step3.device_id);
GRANT SELECT ON phoenix_next_funnel TO looker;
GRANT SELECT ON phoenix_next_funnel TO jjensen;
GRANT SELECT ON phoenix_next_funnel TO jli;
GRANT SELECT ON phoenix_next_funnel TO shasan;