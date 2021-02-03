declare @cutoff as Int
set @cutoff = 0

select distinct
	NHSNumber = COALESCE(Encounter.NHSNumber, Episode.NHSNumber)
	,CONVERT(VARCHAR,
					CASE
						WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime) <= @cutoff) 
						THEN EncounterGlobal.AdmissionTime
						ELSE COVID19.SpecimenCollectionDateTime
			END, 120) as HospitalAdmissionTime /* For community COVID we want admission date, for nosocomial we want swab date*/
	,CONVERT(VARCHAR,
					CASE
						WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime) <= @cutoff) 
						THEN
					CASE
						WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, Episode.StartTime) > 0)
						THEN Episode.StartTime
						ELSE CASE
								WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, Episode.DischargeTime) > 0) OR (Episode.DischargeTime is NULL AND Episode.StartTime is not NULL)
								THEN EncounterGlobal.AdmissionTime
								ELSE NULL
							 END
						END
						ELSE
					CASE
						WHEN (DATEDIFF(day, COVID19.SpecimenCollectionDateTime, Episode.StartTime) > 0)
						THEN Episode.StartTime
						ELSE CASE
								WHEN (DATEDIFF(day, COVID19.SpecimenCollectionDateTime, Episode.DischargeTime) > 0) OR (Episode.DischargeTime is NULL AND Episode.StartTime is not NULL)
								THEN COVID19.SpecimenCollectionDateTime
								ELSE NULL
							 END
						END
			END, 120) as StartTimeCriticalCare /* We want critical care admissions to be on or after "admission", so if nosocomial might need adjusting if testing positive on ICU*/
	,CONVERT(VARCHAR,
					CASE
						WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime) <= @cutoff) 
						THEN
					CASE
						WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, Episode.StartTime) > 0)
						THEN Episode.DischargeTime
						ELSE CASE
								WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, Episode.DischargeTime) > 0) OR (Episode.DischargeTime is NULL AND Episode.StartTime is not NULL)
								THEN Episode.DischargeTime
								ELSE NULL
							 END
						END
						ELSE
					CASE
						WHEN (DATEDIFF(day, COVID19.SpecimenCollectionDateTime, Episode.StartTime) > 0)
						THEN Episode.DischargeTime
						ELSE CASE
								WHEN (DATEDIFF(day, COVID19.SpecimenCollectionDateTime, Episode.DischargeTime) > 0) OR (Episode.DischargeTime is NULL AND Episode.StartTime is not NULL)
								THEN Episode.DischargeTime
								ELSE NULL
							 END
						END
			END, 120) as DischargeTimeCriticalCare /*Only interested in critical care discharges that occur before discharge and after "admission", where admission is adjusted to be swab date for nosocomial cases */
	,HospitalDischargeTime = EncounterGlobal.DischargeTime 
	,Encounter.DateOfDeath
	,Encounter.DateOfBirth
	--,CONVERT(int,
	--				CASE
	--					WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime) <= 2)
	--					THEN Encounter.DateOfBirth
	--					ELSE '3000-01-01'
	--		END) as DateOfBirth /* For nosocomial cases we set DoB as 01/01/3000 as an indicator variable */
	--/*,Encounter.PatientCategoryCode*/
	--,CONVERT(int,
	--				CASE
	--					WHEN (DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime) <= 2)
	--					THEN 0
	--					ELSE 1
	--		END) as noso_flag /* Indicator for sense checking, DoB is used as indicator in the python code */
	/*,COVID19.SpecimenCollectionDateTime    */          /*Uncomment if you want to include the date at which people receive a positive test for COVID - Python code would need modification to include this*/ 

	
	
From
	ModelAPC.Dimension.Encounter


LEFT JOIN ModelAPC.Dimension.EncounterGlobal
ON EncounterGlobal.Recno = Encounter.Recno

/* Unsure if we need the below three inner joins */
inner join ModelAPC.Dimension.DischargeDestination
 on	DischargeDestination.SourceValueID = Encounter.DischargeDestinationID

inner join ModelAPC.Dimension.AdmissionSource
on AdmissionSource.SourceValueID = Encounter.AdmissionSourceID

inner join ModelAPC.Dimension.Sex
on Sex.SourceValueID = Encounter.SexID

/*Matching critical care episodes to APC events - start */
LEFt JOIN [MatchCriticalCare].[Match].[MatchTypeDataset]
ON [TargetDatasetRecno] = Encounter.Recno AND MatchTypeID = 600

LEFT Join ModelCriticalCare.Dimension.Episode
ON Episode.Recno = SourceDatasetRecno

/*LEFT JOIN ModelCriticalCare.Dimension.Episode
ON APCEncounterSourceUniqueID = Encounter.SourceUniqueID OR Episode.NHSNumber = Encounter.NHSNumber*/
/*Matching critical care episodes to APC events - end */

/* Only select patients with a positive PCR COVID test */
inner join SandboxUoM.dbo.COVID19  /* Bespoke table created for UoM/Manchester Foundation Trust access - replace with corresponding table displaying COVID test information! */
ON COVID19.NHSNumber = Encounter.NHSNumber


/* Extract earliest test time for each patient, to facilitate identification of nosocomial infection */
inner join (
			select 
			COVID19.NHSNumber,
			min(COVID19.SpecimenCollectionDateTime) as MinTestTime
			From
			ModelAPC.Dimension.Encounter

			LEFT JOIN ModelAPC.Dimension.EncounterGlobal
			ON EncounterGlobal.Recno = Encounter.Recno
			inner join SandboxUoM.dbo.COVID19  /* Bespoke table created for UoM/Manchester Foundation Trust access - replace with corresponding table displaying COVID test information! */
			ON COVID19.NHSNumber = Encounter.NHSNumber
			where
			(Positive = 1)
			AND
			(
			(
			(DATEDIFF(day, COVID19.SpecimenCollectionDateTime, EncounterGlobal.AdmissionTime) between 0 and 14)
			--OR
			--(DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime) between 0 and 2)
			)
			OR
			(
			(DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime ) >= 0) AND
			((DATEDIFF(day, COVID19.SpecimenCollectionDateTime, EncounterGlobal.DischargeTime ) >= 0) OR EncounterGlobal.DischargeTime is null)
			)
			)
			AND EncounterGlobal.AdmissionDate > '2020-03-01' 
			GROUP BY COVID19.NHSNumber
			) t ON t.NHSNumber = Encounter.NHSNumber AND t.MinTestTime = COVID19.SpecimenCollectionDateTime
			
			
/* Criteria for inclusion in the sample */
WHERE  (Positive = 1)
AND
(
(
(DATEDIFF(day, COVID19.SpecimenCollectionDateTime, EncounterGlobal.AdmissionTime) between 0 and 14)
--OR
--(DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime) between 0 and 2)
)
OR 
(
(DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime ) >= 0) AND
((DATEDIFF(day, COVID19.SpecimenCollectionDateTime, EncounterGlobal.DischargeTime ) >= 0) OR EncounterGlobal.DischargeTime is null)
)
)
AND EncounterGlobal.AdmissionDate > '2020-03-01'  /* This is if we want a date cutoff */
/*AND (EncounterGlobal.AdmissionDate < Episode.StartTime OR Episode.StartTime is null)
AND (EncounterGlobal.DischargeDate > Episode.DischargeTime OR Episode.DischargeTime is null)*/
ORDER BY NHSNumber asc, StartTimeCriticalCare asc; /* Ordering matters for python code - multiple critical care episodes should be listed in chronological order */