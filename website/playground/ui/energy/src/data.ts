const millionBtuPerKilowattHour = 3412 / 1000000;
const thousandCubicFeetPerMillionBtu = (1039 * 1000) / 1000000; // https://www.nrg.com/resources/energy-tools/energy-conversion-calculator.html

export const averageCarbonEmissionsInKilogramsPerHouseholdPerMonth = 4000; // https://css.umich.edu/publications/factsheets/sustainability-indicators/carbon-footprint-factsheet, converted from t/yr to kg/mo

export const energySources = [
    {
        name: "Coal",
        image: "https://img.icons8.com/color/48/manufacturing.png",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const costInDollarsPerMillionBtu = 2.37;
            return kilowattHours * millionBtuPerKilowattHour * costInDollarsPerMillionBtu;
        },
        costSource:
            "https://www.statista.com/statistics/244479/us-consumer-price-estimates-for-coal-energy/",
        carbonEmissionsInKilogramsPerKilowattHours: (kilowattHours: number) => {
            const kilogramsPerMillionBtu = 95.92;
            return kilowattHours * millionBtuPerKilowattHour * kilogramsPerMillionBtu;
        },
        emissionsSource: "https://www.eia.gov/environment/emissions/co2_vol_mass.php",
        info: "![Diagram of coal-fired power plant](https://energyeducation.ca/wiki/images/5/54/Coal_fired_power_plant_diagram.svg.png)\n\nCoal is used to generate energy through the [process of burning](https://www.usgs.gov/faqs/what-coal-used). The heat generated through coal combustion is used to generate steam which in return generates electricity.\n\n![Coal burning](https://illuminem.b-cdn.net/articlebody/a8c3da0ff1c1bec7b1e54f676b9c9c3c538d400c.jpg)\n\nSimilarly, to natural gas, coal is an abundant source of energy, with there being [1.16 trillion short tons](https://illuminem.com/illuminemvoices/examining-the-pros-and-cons-of-coal-as-an-energy-source) of coal reserves on the planet. Additionally, coal is cheaper than other fossil fuels which makes it easier to distribute worldwide. At the same time, mining coal can lead to water pollution. Mining coal can cause acid mine drainage which runs into water and can disrupt the growth and reproduction of aquatic plants. [Coal mining](https://illuminem.com/illuminemvoices/examining-the-pros-and-cons-of-coal-as-an-energy-source) can also cause destruction to natural habitats as coal mining requires land clearing and excavation. ",
    },
    {
        name: "Natural Gas",
        image: "https://img.icons8.com/color/96/gas-industry.png",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const costInDollarsPerThousandCubicFeet = 18.88;
            return (
                kilowattHours *
                millionBtuPerKilowattHour *
                costInDollarsPerThousandCubicFeet *
                thousandCubicFeetPerMillionBtu
            );
        },
        costSource: "https://www.chooseenergy.com/data-center/natural-gas-rates-by-state/", // Massachusetts
        carbonEmissionsInKilogramsPerKilowattHours: (kilowattHours: number) => {
            const kilogramsPerMillionBtu = 52.91;
            return kilowattHours * millionBtuPerKilowattHour * kilogramsPerMillionBtu;
        },
        emissionsSource: "https://www.eia.gov/environment/emissions/co2_vol_mass.php",
        info: "![Diagram of natural gas power plant](https://energyeducation.ca/wiki/images/3/38/Simple_cycle_plant_update.png)\n\nNatural gas is converted into energy by being extracted from the ground and used by [boilers to create steam](http://energyforprogress.org/article/one-question-on-energy-how-does-natural-gas-become-electricity/). This steam is then used to spin a turbine which generates electricity. Alternatively, a combustion turbine is used to create a rotating mass to create electricity.\n\n![Natural gas burning](https://images.nationalgeographic.org/image/upload/v1638886354/EducationHub/photos/natural-gas.jpg)\n\nWhile not renewable, natural gas is an abundant source of energy. Natural gas production reached a record high level of 79 billion cubic feet per day in 2015 so it is unlikely we’ll run out of it soon. While not entirely clean, [natural gas emits half of the carbon dioxide that coal does when burned.](https://yaleclimateconnections.org/2016/07/pros-and-cons-the-promise-and-pitfalls-of-natural-gas/) While plentiful, this does not change the fact that natural gas is a nonrenewable source of energy. At some point, we will run out of natural gas. Another downside is that natural gas leaks emit huge amounts of methane into the atmosphere, almost [100,000 tons](https://yaleclimateconnections.org/2016/07/pros-and-cons-the-promise-and-pitfalls-of-natural-gas/).",
    },
    {
        name: "Solar",
        image: "https://img.icons8.com/color/96/solar-panel.png",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const constInDollarsPerKilowattHour = 0.049;
            return kilowattHours * constInDollarsPerKilowattHour;
        },
        costSource:
            "https://www.irena.org/Publications/2023/Aug/Renewable-Power-Generation-Costs-in-2022",
        carbonEmissionsInKilogramsPerKilowattHours: undefined,
        emissionsSource: undefined,
        info: "![Diagram of solar power system](https://neumanncompanies.com/wp-content/uploads/2022/01/SunVest_solar-diagram_21.jpg)\n\nSolar energy is energy taken in from the sun and converted into energy. The panels contain [photovoltaic](https://www.energy.gov/eere/solar/solar-photovoltaic-technology-basics) cells which [absorb the energy](http://www.energy.gov/eere/solar/how-does-solar-work) and create electrical charges within said cell that end up causing electricity to flow.\n\n![Solar panels](https://images.unsplash.com/photo-1613665813446-82a78c468a1d)\n\nSolar panels provide continuous energy throughout the day, meaning on days that are bright and sunny you’ll be able to collect lots of energy. Solar batteries can be purchased to store energy so that on days when the sun might not be out as much, you can still have energy flowing into your house. While that may seem beneficial, buying a solar battery may cost anywhere from [\\$12,000 to \\$22,000](https://www.energy.gov/eere/solar/articles/should-i-get-battery-storage-my-solar-energy-system) by itself. With that steep of a price, for those who do not own a solar battery, days that the sun isn’t out as much may be disastrous without a way to generate electricity from the solar panels.",
    },
    {
        name: "Wind",
        image: "https://img.icons8.com/color/96/wind-turbine.png",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const constInDollarsPerKilowattHour = 0.081;
            return kilowattHours * constInDollarsPerKilowattHour;
        },
        costSource:
            "https://www.irena.org/Publications/2023/Aug/Renewable-Power-Generation-Costs-in-2022",
        carbonEmissionsInKilogramsPerKilowattHours: undefined,
        emissionsSource: undefined,
        info: "![Diagram of wind turbine](https://loeriesfonteinwind.co.za/wp-content/uploads/2015/07/Windfarm-Diagram-585x392.jpg)\n\nWind energy is energy that is generated from wind turbines. These wind turbines generate electricity from the [kinetic energy of the wind](https://www.energy.gov/eere/wind/wind-energy-basics).\n\n![Wind turbines](https://images.unsplash.com/photo-1548337138-e87d889cc369)\n\nOne of the benefits of wind turbines is that they can be placed in various locations, meaning that there is flexibility in which they are placed. Wind turbines, as the name would suggest, rely on wind which is a renewable energy source. This would mean that it would be impossible to exhaust this resource. While this is beneficial, wind turbines are similar to solar panels. If a given day is not windy, then the amount of energy will be drastically reduced. Another drawback is that while wind turbines can be placed anywhere, the locations can also [affect wildlife](https://www.energy.gov/eere/wind/advantages-and-challenges-wind-energy). The placing of the turbines can uproot the lives of the animals and plants living there.",
    },
];
