# Función para calcular el valor futuro de una anualidad anticipada
valor_futuro_anualidad <- function(anualidad, tasa, n) {
  return(anualidad * ((1 + tasa)^n - 1) / tasa)
}

# Función para calcular la anualidad a partir del valor futuro
anualidad_con_valor_futuro <- function(valor_futuro, tasa, n) {
  return(valor_futuro * tasa / ((1 + tasa)^n - 1))
}

# Función para calcular el número de pagos necesarios para alcanzar un valor futuro
plazo_con_valor_futuro <- function(valor_futuro, anualidad, tasa) {
  return(ceiling(log((valor_futuro * tasa / anualidad) + 1) / log(1 + tasa)))
}

# Función para calcular la tasa de interés del periodo a partir del valor futuro
tasa_con_valor_futuro <- function(valor_futuro, n, anualidad) {
  lower_bound <- 0.0
  upper_bound <- 1.0
  epsilon <- 1e-6  # Precisión deseada
  while (upper_bound - lower_bound > epsilon) {
    tasa <- (lower_bound + upper_bound) / 2
    vf_calculado <- valor_futuro_anualidad(anualidad, tasa, n)
    if (vf_calculado < valor_futuro) {
      lower_bound <- tasa  # Necesitamos una tasa mayor
    } else {
      upper_bound <- tasa  # Necesitamos una tasa menor
    }
  }
  return(tasa)
}

# Función para calcular el valor actual de una anualidad anticipada
valor_actual_anualidad <- function(anualidad, tasa, n) {
  return(anualidad * (1 - (1 + tasa)^-n) / tasa)
}

# Función para calcular la anualidad a partir del valor actual
anualidad_con_valor_actual <- function(valor_actual, tasa, n) {
  return(valor_actual * tasa / (1 - (1 + tasa)^-n))
}

# Función para calcular el número de pagos necesarios para alcanzar un valor actual
plazo_con_valor_actual <- function(valor_actual, anualidad, tasa) {
  return(ceiling(log((anualidad / tasa) / (anualidad / tasa - valor_actual)) / log(1 + tasa)))
}

# Función para calcular la tasa de interés del periodo a partir del valor actual
tasa_con_valor_actual <- function(valor_actual, n, anualidad) {
  lower_bound <- 0.0
  upper_bound <- 1.0
  epsilon <- 1e-6  # Precisión deseada
  while (upper_bound - lower_bound > epsilon) {
    tasa <- (lower_bound + upper_bound) / 2
    va_calculado <- valor_actual_anualidad(anualidad, tasa, n)
    if (va_calculado < valor_actual) {
      lower_bound <- tasa  # Necesitamos una tasa mayor
    } else {
      upper_bound <- tasa  # Necesitamos una tasa menor
    }
  }
  return(tasa)
}


