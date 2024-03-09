select * from CovidDeaths
where continent is not null
order by 3,4

select * from CovidVaccinations
where continent is not null
order by 3,4

--select the data that we are going to be using
select location, date, total_cases, new_cases, total_deaths, population
from CovidDeaths
where continent is not null
order by 1,2

--looking at total cases vs total deaths
--shows the likelihood dyimg if you contract covid in your country
select location, date, total_cases, total_deaths, (total_deaths/total_cases) * 100 as DeathPercentage
from CovidDeaths
where location like '%states%'
order by 1,2

--looking at total cases Vs population
--shows what percentage of population got covid
select location, date, total_cases, population, (total_cases/population) * 100 as PercentPopulationInfected
from CovidDeaths
--where location like '%states%'
order by 1,2

--looking at countries with highest infection rate compared to population
select location, population, MAX(total_cases) as HighestInfectionCount, MAX((total_cases/population))* 100 as PercentPopulationInfected
from CovidDeaths
where continent is not null
group by population, location
order by PercentPopulationInfected desc


--showing the countries with highestdeath count per population
select location, MAX(cast(total_deaths as int)) as TotalDeathCount
from CovidDeaths
where continent is not null
group by location
order by TotalDeathCount desc

--LETS BREAK THINGS DOWN BY CONTINENT
--showing the continents with highest death count
select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
from CovidDeaths
where continent is not null
group by continent
order by TotalDeathCount desc



--Global Numbers
select date, sum(new_cases) as TotalCases, sum(cast(new_deaths as int)) as TotalDeaths, 
	sum(cast(new_deaths as int))/sum(new_cases)*100 as DeathPercentage                
from CovidDeaths
where continent is not null
group by date
order by 1,2

select sum(new_cases) as TotalCases, sum(cast(new_deaths as int)) as TotalDeaths, 
	sum(cast(new_deaths as int))/sum(new_cases)*100 as DeathPercentage                
from CovidDeaths
where continent is not null
--group by date
order by 1,2


--looking at total population vs vaccinations
select D.continent, D.location, D.date, D.population, V.new_vaccinations, sum(convert(int, V.new_vaccinations)) OVER (PARTITION BY D.location 
order by D.location, D.date) as RollingPeopleVaccinated
--,(RollingPeopleVaccinated/population)*100
from CovidDeaths D
join CovidVaccinations V on D.location = V.location 
						and D.date = V.date
where D.continent is not null
order by 2,3



--USE CTE

--looking at total population vs vaccinations
with PopvsVac (continent, location, date, population, NewVaccinations, RollingPeopleVaccinated)
as 
(
select D.continent, D.location, D.date, D.population, V.new_vaccinations as NewVaccinations, sum(convert(int, V.new_vaccinations)) OVER (PARTITION BY D.location 
order by D.location, D.date) as RollingPeopleVaccinated
--,(RollingPeopleVaccinated/population)*100
from CovidDeaths D
join CovidVaccinations V on D.location = V.location 
						and D.date = V.date
where D.continent is not null
--order by 2,3
)
select *, (RollingPeopleVaccinated/population)*100 
from PopvsVac





--TEMP TABLE
drop table if exists #PercentPopulationVaccinated
create table #PercentPopulationVaccinated
(
continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
RollingPeopleVaccinated numeric
)

insert into #PercentPopulationVaccinated
select D.continent, D.location, D.date, D.population, V.new_vaccinations as NewVaccinations, sum(convert(int, V.new_vaccinations)) OVER (PARTITION BY D.location 
order by D.location, D.date) as RollingPeopleVaccinated
--,(RollingPeopleVaccinated/population)*100
from CovidDeaths D
join CovidVaccinations V on D.location = V.location 
						and D.date = V.date
--where D.continent is not null
--order by 2,3
select *, (RollingPeopleVaccinated/population)*100 
from #PercentPopulationVaccinated






--cretaing view to store data for later visualizations

create view PercentPopulationVaccinated as 
select D.continent, D.location, D.date, D.population, V.new_vaccinations as NewVaccinations, 
sum(convert(int, V.new_vaccinations)) OVER (PARTITION BY D.location 
order by D.location, D.date) as RollingPeopleVaccinated
--,(RollingPeopleVaccinated/population)*100
from CovidDeaths D
join CovidVaccinations V on D.location = V.location 
						and D.date = V.date
where D.continent is not null
--order by 2,3


select * from PercentPopulationVaccinated






