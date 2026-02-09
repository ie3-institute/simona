(price_service)=

# Price service

The price parses a price time series containing wholesale price data (e.g. day-ahead or intra-day).
Please refer to {doc}`PowerSystemDataModel - Time Series <psdm:models/input/additionaldata/timeseries>` for the used attributes and units and to the [configuration page](../config.md#price-data) for hints how to configure the service.

Generally, in order to derive prosumer prices, the fees are added/subtracted to the wholesale price, after which the tax is added/subtracted.

## Example scenario

Here we showcase an exemplary price configuration for Dortmund in 2025.
Installations costs (capex) and recurring fixed costs, such as metering point operation (Messstellenbetrieb) and the basic charge of grid fees (Grundpreis der Netzentgelte), are not considered.
This is because we are interested in optimizing the use of flexibility, not the cost of the installation setup.

- Buying price
    - Fees
        - Grid fees (Netzentgelte): 7.17 ct/kWh
        - Electricity tax (Stromsteuer): 2.05 ct/kWh
        - EEG-Umlage: 0 ct/kWh
        - Offshore-Umlage: 0.816 ct/kWh
        - NEV-Umlage/Aufschlag für besondere Netznutzung: 1.558 ct/kWh
        - KWKG-Umlage: 0.277 ct/kWh
        - Concession levy (Konzessionsabgabe): 2.39 ct/kWh
        - Supplier margin (Lieferantenmarge): 4.45 ct/kWh
    - Tax
        - Sales tax (VAT/Umsatzsteuer): 19%
- Selling price
    - Fees
        - Fixed EEG remuneration is not considered
        - Not standardized, thus we assume 0.5 ct/kWh
    - Tax
        - No sales tax (VAT/Umsatzsteuer): 0%
            - For household installations, most probably small business regulation (Kleinunternehmerregelung) can be used, which means that no VAT has to be applied to sold goods.
            - In other cases:
                - For sold energy, VAT is a transit item (Durchlaufender Posten), which means VAT is not relevant to the prosumer's profits.
                - For energy consumed by the private household, VAT would need to be paid, but our model does not consider this.
