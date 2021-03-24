import join_now_src as src


join_now_sql = '''
SELECT
    f.*,
    u.northstar_id as created,
    u.created_at
FROM (
    SELECT DISTINCT
        e.session_id,
        e.device_id,
        first_value(event_datetime) OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS landing_datetime,
        first_value("path") OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS landing_page,
        first_value(northstar_id) OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS first_ns,
        LAST_VALUE(northstar_id) OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS last_ns
    FROM phoenix_events_combined e
    JOIN (
        SELECT DISTINCT session_id
        FROM phoenix_events_combined
        WHERE event_name = 'phoenix_clicked_nav_link_join_now'
    ) s ON s.session_id = e.session_id
) f
LEFT JOIN users u ON u.northstar_id = f.last_ns
WHERE first_ns IS NULL
'''

join_now_banner_sql = '''
SELECT
    f.*,
    u.northstar_id as created,
    u.created_at
FROM (
    SELECT DISTINCT
        e.session_id,
        e.device_id,
        first_value(event_datetime) OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS landing_datetime,
        first_value("path") OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS landing_page,
        first_value(northstar_id) OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS first_ns,
        LAST_VALUE(northstar_id) OVER (PARTITION BY e.session_id ORDER BY event_datetime ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS last_ns
    FROM phoenix_events_combined e
    JOIN (
        SELECT DISTINCT session_id
        FROM phoenix_events_combined
        WHERE event_name = 'phoenix_clicked_call_to_action_banner'
    ) s ON s.session_id = e.session_id
) f
LEFT JOIN users u ON u.northstar_id = f.last_ns
WHERE first_ns IS NULL
'''

src.load_data_and_send_email(
    join_now_sql,
    path="/Users/mjain/Desktop/new_account_reports/nav_join_accounts_{}.csv",
    fromaddr='mjain@dosomething.org',
    toaddr=['mjain@dosomething.org', 'ddonizeth@dosomething.org'],
    subject='UPDATED {}: Accounts Created from Nav Join Now')


src.load_data_and_send_email(
    join_now_banner_sql,
    path="/Users/mjain/Desktop/new_account_reports/banner_join_accounts_{}.csv",  # noqa
    fromaddr='mjain@dosomething.org',
    toaddr=['mjain@dosomething.org', 'ddonizeth@dosomething.org'],
    subject='UPDATED {}: Accounts Created from Banner')
