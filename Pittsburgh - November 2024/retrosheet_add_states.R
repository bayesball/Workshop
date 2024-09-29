retrosheet_add_states <- function(data, ...) {
  data |>
    dplyr::mutate(
      bases = paste0(
        ifelse(base1_run_id == "", 0, 1),
        ifelse(base2_run_id == "", 0, 1),
        ifelse(base3_run_id == "", 0, 1)
      ),
      state = paste(bases, outs_ct),
      is_runner1 = as.numeric(
        run1_dest_id == 1 | bat_dest_id == 1
      ),
      is_runner2 = as.numeric(
        run1_dest_id == 2 | run2_dest_id == 2 |
          bat_dest_id == 2
      ),
      is_runner3 = as.numeric(
        run1_dest_id == 3 | run2_dest_id == 3 |
          run3_dest_id == 3 | bat_dest_id == 3
      ),
      new_outs = outs_ct + event_outs_ct,
      new_bases = paste0(is_runner1, is_runner2, is_runner3),
      new_state = paste(new_bases, new_outs),
      runs_scored =
        (bat_dest_id > 3) + (run1_dest_id > 3) +
        (run2_dest_id > 3) + (run3_dest_id > 3)
    )
}
