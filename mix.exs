defmodule TheCornerOfElixir.MixProject do
  use Mix.Project
  @name :"The Corner of Elixir"
  def project do
    [
      app: @name,
      version: "4.0.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      source_url: "https://github.com/keepzen/the-corner-of-elixir",
      homepage_url: "https://keepzen.github.io/the-corner-of-elixir",
      deps: [corner: "https://hexdocs.pm/corner/api-reference.html"],
      docs:
        [
          formatters: ["html"],
          authors: ["Jayson Wu", "Keep Zen"],
          api_reference: false,
          language: ["en", "cn-zh"],
          assets: "assets",
          before_closing_head_tag: &before_closing_head_tag/1,
          main: "readme",
          markdown_processor: {ExDoc.Markdown.Earmark, footnotes: true}
        ] ++ cn_docs()
    ]
  end

  defp cn_docs do
    [
      extras: [
        "ReadMe.md",
        "en/index_en.md"
        # "en/ch01.intruction_en.md",
        # "en/ch02.plus_and_minus_en.md",
        # "en/ch03.pattern_match_en.md",
        # "en/ch04.parenthese_en.md",
        # "en/ch05.new_constructor_en.md",
        # "en/ch06.async_programe_en.md",
        # "en/ch07.pipe_en.md",
        # "en/ch08.error_handle_en.md",
        # "en/ch09.module_en.md",
        # "en/ch10.protocol_and_behaviour_en.md",
        # "en/ch11.macro_en.md",
        # "en/ch12.process_en.md",
        # "en/chx.cold_knowledge_en.md"
      ],
      output: "doc/en"
    ]
  end

  defp before_closing_head_tag(:html) do
    """
    <style>
      a.footnote {
        vertical-align: super;
      }
      a.reversefootnote {
        display: inline-block;
        text-indent: -9999px;
        line-height: 0;
      }
      a.reversefootnote:after {
        content: '↩'; /* or any other text you want */
        text-indent: 0;
        display: block;
        line-height: initial;
      }
    </style>

    <script>
    MathJax = {
    tex: {
    inlineMath: [['$', '$']]
    }
    };
    </script>
    <script id="MathJax-script" async
    src="./assets/tex-chtml.js">
    </script>

    <script src="assets/mermaid.min.js"></script>
    <script>
    document.addEventListener("DOMContentLoaded", function () {
    mermaid.initialize({ startOnLoad: false });
    let id = 0;
    for (const codeEl of document.querySelectorAll("pre code.mermaid")) {
      const preEl = codeEl.parentElement;
      const graphDefinition = codeEl.textContent;
      const graphEl = document.createElement("div");
      const graphId = "mermaid-graph-" + id++;
      mermaid.render(graphId, graphDefinition, function (svgSource, bindListeners) {
        graphEl.innerHTML = svgSource;
        bindListeners && bindListeners(graphEl);
        preEl.insertAdjacentElement("afterend", graphEl);
        preEl.remove();
      });
    }
    });
    </script>
    """
  end

  defp before_closing_head_tag(_), do: ""

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  def deps do
    [
      {:ex_doc, "~> 0.27", only: :dev, runtime: false},
      {:makeup_js, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end
end
