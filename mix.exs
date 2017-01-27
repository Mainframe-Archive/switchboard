defmodule Switchboard.Mixfile do
  use Mix.Project

  def project do
    [app: :switchboard,
     version: "0.3.2",
     compilers: [:erlang, :app],
     erlc_options: [{:parse_transform, :lager_transform}, {:i, "include"}],
     deps: deps()]
  end

  def application do
    [applications: [:kernel,
                    :stdlib,
                    :inets,
                    :crypto,
                    :poolboy,
                    :lager,
                    :gproc,
                    :jsx]]
  end

  defp deps do
    [
     {:lager,    github: "basho/lager"},
     {:poolboy,  github: "devinus/poolboy"},
     {:cowboy,   github: "ninenines/cowboy",  tag: "1.0.3"},
     {:gproc,    github: "uwiger/gproc"},
     {:jsx,      github: "talentdeficit/jsx"},
     {:ranch,    github: "ninenines/ranch",   tag: "1.1.0", override: true}
    ]
  end
end
