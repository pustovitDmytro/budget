FROM pustovitdmytro/budget-base:1.0.1

WORKDIR /app

COPY src /app
COPY budget.Rproj /app/budget.Rproj