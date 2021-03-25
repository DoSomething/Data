-- IVR Request
-- This was the SQL used for data that we sent to the Independent Voter Research project for our 2020 VR Efforts
-- Participation in the study will mean we get market information back and see how we stacked up
-- and have an understanding of other successful tactics from other organizations

-------------
-- Done: Ingest up to date file (2018-11-07- 2020-11-03) https://register.rockthevote.com/partner/reports
-- Done: Required fields to include: First name, Last name, Address, Apt #, City, State, Zip, Program Type (*), Registration Date (YYYY-MM-DD)
-- Done: Filter for Completed
-- Done: Pull out tracking source details
-- Done: Map source
		-- ONLINE - SMS										vr_source in ('sms')
		-- ONLINE - Paid Digital Ads						vr_source in ('ads')
		-- ONLINE - Relational  (OVRD, refer a friend)		vr_source in
		-- ONLINE - Partnerships  (partnerships)			vr_source in ('influencer', 'marketingpartner')
		-- ONLINE - Program   (internal stuff, quiz etc)    vr_source in ('email', 'social', 'web', 'company'
		-- ONLINE - Organic   (no attribution)				vr_source is null or everything else

-- count records
select count(*) from analyst_sandbox."registrants-report_20181107_20201103"  562,047

-- preview fields
select * from analyst_sandbox."registrants-report_20181107_20201103" limit 100

-- start with completed_records only and parse tracking_source
with completed_registrations as
	(select
	 "First name" as first_name
	,"Last name"  as last_name
	,"Home address" as address
	,"Home unit" as apt_num
	,"Home city"  as city
	,"Home state" as state
	,"Home zip code" as zip
	, "Started registration"::timestamptz as registration_date
	, case
		when "Tracking Source"='ads' then 'ads'
		else split_part(substring("Tracking Source" from 'source\:(.+)'), ',', 1)
		 end as vr_source
	, split_part(substring("Tracking Source" from 'source_details\:(.+)'), ',', 1) as vr_source_details
	from analyst_sandbox."registrants-report_20181107_20201103"
	where status= 'Complete'
	)

--Check values before mapping
	-- select vr_source, vr_source_details from completed_registrations group by 1,2

, mapped_registrations as

	(select *
	, case when vr_source='sms' then 'ONLINE - SMS'
	       when vr_source='ads' then 'ONLINE - Paid Digital Ads'
	       when vr_source_details ilike '%OnlineRegistrationDrive%' then 'ONLINE - Relational'
	       when vr_source in ('influencer', 'marketingpartner') then 'ONLINE - Partnerships'
	       when vr_source in ('email', 'social', 'web', 'company') then 'ONLINE - Program'
	        else 'ONLINE - Organic' end as program_type
	from completed_registrations )

select * from mapped_registrations
