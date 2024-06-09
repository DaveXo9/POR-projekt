rm(list=ls())
generate_chaotic_sequence <- function(initial_value, r, n) {
  x <- numeric(n)
  x[1] <- initial_value
  for (i in 2:n) {
    x[i] <- r * x[i-1] * (1 - x[i-1])
  }
  return(x)
}

encode_message <- function(message, chaotic_seq) {
  ascii_values <- as.integer(charToRaw(message))
  chaotic_ints <- as.integer(chaotic_seq * 256) 
  encoded_values <- bitwXor(ascii_values, chaotic_ints[1:length(ascii_values)])
  return(encoded_values)
}

decode_message <- function(encoded_message, chaotic_seq) {
  chaotic_ints <- as.integer(chaotic_seq * 256)
  decoded_values <- bitwXor(encoded_message, chaotic_ints[1:length(encoded_message)])
  decoded_message <- rawToChar(as.raw(decoded_values))
  return(decoded_message)
}

initial_value <- 0.5
r <- 3.99
message <- readline(prompt="Enter the message to encode: ")
n <- nchar(message)

chaotic_seq <- generate_chaotic_sequence(initial_value, r, n)

encoded_message <- encode_message(message, chaotic_seq)
print(paste("Encoded Message: ", toString(encoded_message)))

decoded_message <- decode_message(encoded_message, chaotic_seq)
print(paste("Decoded Message: ", decoded_message))
