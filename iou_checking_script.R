ious <- conspiracies %>% 
  dplyr::select(W2_IOU1:W2_IOU12,W2_IOU_Total)


# CRT
iou_keys <- list(iou = cs(W2_IOU1,W2_IOU2,W2_IOU3,W2_IOU4,W2_IOU5,
                          W2_IOU6,W2_IOU7,W2_IOU8,W2_IOU9,W2_IOU10,
                          W2_IOU11,W2_IOU12))

iou_test <- scoreItems(iou_keys, ious, min = 1, max = 5)
head(iou_test$scores)
iou_test$alpha  # Scale alpha
ious$iou_total <- rescale01(iou_test$scores, na.rm = TRUE)
ious$iou_total <- c(ious$iou_total)  # Ensure variable is numeric and not matrix class

describe(ious$iou_total)

ious %>% 
  ggplot(aes(x = iou_total, y = W2_IOU_Total)) +
  geom_point()

ious$W2_IOU_Total <- rescale01(ious$W2_IOU_Total, na.rm = TRUE)

mean(round(ious$W2_IOU_Total,3) == round(ious$iou_total,3))

cor(ious$W2_IOU_Total, ious$iou_total)

vars <- names(ious)[1:12]

ious[vars] <- ious[vars] %>% 
  map_df(to_factor)

labs <- c(
  "Unforeseen events upset me",
  "It frustrates me not having all the information I need",
  "One should always look ahead so as to avoid surprises",
  "A small, unforeseen event can spoil everything, even with planning",
  "I always want to know what the future has in store for me",
  "I can't stand being taken by surprise",
  "I should be able to organize everything in advance",
  "Uncertainty keeps me from living a full life",
  "When it's time to act, uncertainty paralyses me",
  "When I am uncertain I can't function very well",
  "The smallest doubt can stop me from acting",
  "I must get away from all uncertain situations"
)

for(i in 1:12){
  x1 <- ious[vars][i] %>% as_vector()
  y <- ious$W2_IOU_Total
  
  print(
    ggplot(data = NULL, aes(x = as.factor(x1),
                            y = y)) +
      geom_boxplot() +
      coord_flip() +
      labs(x = labs[i], y = "IOU Total")
    )
}
