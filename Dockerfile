FROM pustovitdmytro/budget-base:1.1.0

WORKDIR /app

COPY src /app
COPY budget.Rproj /app/budget.Rproj