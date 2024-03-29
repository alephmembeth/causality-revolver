library(effsize)
library(stats)
library(tidyverse)
library(xtable)


# data import
causality_revolver <- haven::read_dta("data.dta")
sytsma_livengood_797 <- read.delim("sytsma_livengood_797.txt", comment.char = "#")
sytsma_livengood_798 <- read.delim("sytsma_livengood_798.txt", comment.char = "#")


# vector of the neutral value 4
neutral_rev = rep(4, nrow(causality_revolver))


# tests
wilcox.test(x = causality_revolver$ah, mu = 4)
wilcox.test(x = causality_revolver$bh, mu = 4)
wilcox.test(x = causality_revolver$dh, mu = 4)
wilcox.test(x = causality_revolver$eh, mu = 4)
wilcox.test(x = causality_revolver$fh, mu = 4)
wilcox.test(x = causality_revolver$gh, mu = 4)

cliff.delta(causality_revolver$ah, neutral_rev)
cliff.delta(causality_revolver$bh, neutral_rev)
cliff.delta(causality_revolver$dh, neutral_rev)
cliff.delta(causality_revolver$eh, neutral_rev)
cliff.delta(causality_revolver$fh, neutral_rev)
cliff.delta(causality_revolver$gh, neutral_rev)

revolver_all <- causality_revolver %>%
   dplyr::select(
      id,
      ab,
      ac,
      ad,
      ae,
      af,
      ag,
      ah,
      bc,
      bd,
      be,
      bf,
      bg,
      bh,
      cd,
      ce,
      cf,
      cg,
      ch,
      de,
      df,
      dg,
      dh,
      ef,
      eg,
      eh,
      fg,
      fh,
      gh
   ) %>%
   tidyr::pivot_longer(-id, names_to = "case", values_to = "value") %>%
   mutate(
      case = recode(
         case,
         `ab` = "A/B",
         `ac` = "A/C",
         `ad` = "A/D",
         `ae` = "A/E",
         `af` = "A/F",
         `ag` = "A/G",
         `ah` = "A/H",
         `bc` = "B/C",
         `bd` = "B/D",
         `be` = "B/E",
         `bf` = "B/F",
         `bg` = "B/G",
         `bh` = "B/H",
         `cd` = "C/D",
         `ce` = "C/E",
         `cf` = "C/F",
         `cg` = "C/G",
         `ch` = "C/H",
         `de` = "D/E",
         `df` = "D/F",
         `dg` = "D/G",
         `dh` = "D/H",
         `ef` = "E/F",
         `eg` = "E/G",
         `eh` = "E/H",
         `fg` = "F/G",
         `fh` = "F/H",
         `gh` = "G/H",
      )
   ) %>% mutate(own_study = TRUE)

original_revolver_data <- sytsma_livengood_797 %>%
   rename(
      "A/H" = Trent,
      "B/H" = Hammer,
      "D/H" = Powder,
      "F/H" = Bullet,
   ) %>% mutate(id = row_number() + 52,
      "E/H" = `D/H`,
      "G/H" = `F/H`,
   ) %>%
   tidyr::pivot_longer(-id, names_to = "case", values_to = "value") %>%
   mutate(own_study = FALSE)

revolver_all %>%
   bind_rows(original_revolver_data) %>%
   filter(case %in% c("A/H", "B/H", "D/H", "E/H", "F/H", "G/H")) %>%
   mutate(
      case = recode(
         case,
         `A/H` = "1 vs. A/H",
         `B/H` = "2 vs. B/H",
         `D/H` = "3 vs. D/H",
         `E/H` = "3 vs. E/H",
         `F/H` = "4 vs. F/H",
         `G/H` = "4 vs. G/H",
      )
   ) %>%
   group_by(own_study, case, value) %>%
   summarise(n = n()) %>%
   mutate(freq = n / sum(n)) %>%
   ungroup() %>%
   complete(own_study, case, value, fill = list(n = 0, freq = 0)) %>%
   ggplot2::ggplot(ggplot2::aes(x = value, y = freq, fill = own_study)) +
   ggplot2::geom_bar(color = "black",
      position = "dodge",
      stat = "identity") +
   facet_wrap( ~ case, scales = "free") +
   scale_fill_manual(
      name = "",
      labels = c("Livengood & Sytsma", "Our Study"),
      values = c("#ffffff", "#000000")
   ) +
   scale_x_continuous(breaks = seq(1, 7, by = 1)) +
   scale_y_continuous(limits = c(0, 0.9), breaks = seq(0, 0.8, by = 0.2)) +
   xlab("Response") +
   ylab("Relative Frequency") +
   theme_bw() +
   theme(legend.position = "top")

ggsave(
   "../preprint/figures/comparison.pdf",
   width = 14.43,
   height = 11.29,
   units = "cm"
)


# p values
revolver_all %>%
   dplyr::group_by(case) %>%
   dplyr::summarise(value = list(value)) %>%
   dplyr::group_by(case) %>%
   dplyr::mutate(
      v_value = wilcox.test(unlist(value), mu = 4)$statistic,
      p_value = wilcox.test(unlist(value), mu = 4)$p.value
   ) %>%
   select(-value) %>%
   dplyr::ungroup() %>%
   mutate(p_value_adjusted = stats::p.adjust(p_value, method = "fdr")) %>%
   mutate(
      p_value2 = case_when(
         p_value < 0.001 ~ paste0("$< 0.001$\\sym{***}"),
         p_value < 0.01 ~ paste0("$\\phantom{< }", round(p_value, 3), "$\\sym{**\\phantom{*}}"),
         p_value < 0.05 ~ paste0(round(p_value, 3), "$\\sym{*\\phantom{**}}"),
         TRUE ~ formatC(p_value, digits = 3)
      ),
      p_value_adjusted2 = case_when(
         p_value_adjusted < 0.001 ~ paste0("$< 0.001$\\sym{***}"),
         p_value_adjusted < 0.01 ~ paste0(
            "$\\phantom{< }",
            round(p_value_adjusted, 3),
            "$\\sym{**\\phantom{*}}"
         ),
         p_value_adjusted < 0.05 ~ paste0(
            "$\\phantom{< }",
            round(p_value_adjusted, 3),
            "$\\sym{*\\phantom{**}}"
         ),
         TRUE ~ formatC(p_value_adjusted, digits = 3)
      ),
   ) %>%
   select(-p_value_adjusted,-p_value,-p_value2) %>%
   rename("Case" = case,
      "$V$" = v_value,
      "Adjusted $p$-value" = p_value_adjusted2,) -> table_data

print(
   xtable(table_data, type = "latex"),
   floating = FALSE,
   latex.environments = NULL,
   booktabs = TRUE,
   file = "../preprint/wilcoxon.tex",
   sanitize.text.function = function(x) {
      x
   },
   include.rownames = FALSE
)
