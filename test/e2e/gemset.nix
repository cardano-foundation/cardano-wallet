{
  bip_mnemonic = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0p9kdsy0i6x4170l83726lrm52c1g62pfzi6ip3nx0ysj6h3qffy";
      type = "gem";
    };
    version = "0.0.4";
  };
  blake2b = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1i7kxnnvv2lqglc1crhkqp0s9hybx20wgrl04jqkk7y2sawyb6hg";
      type = "gem";
    };
    version = "0.10.0";
  };
  cardano_wallet = {
    dependencies = ["bip_mnemonic" "httparty"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1az3d89mlkvy53mfdaxd73y91m1v9z96zsilvkdgagfcf21ywisj";
      type = "gem";
    };
    version = "0.4.0";
  };
  cbor = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0511idr8xps9625nh3kxr68sdy6l3xy2kcz7r57g47fxb1v18jj3";
      type = "gem";
    };
    version = "0.5.9.6";
  };
  diff-lcs = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0rwvjahnp7cpmracd8x732rjgnilqv2sx7d1gfrysslc3h039fa9";
      type = "gem";
    };
    version = "1.5.0";
  };
  httparty = {
    dependencies = ["mime-types" "multi_xml"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "17gpnbf2a7xkvsy20jig3ljvx8hl5520rqm9pffj2jrliq1yi3w7";
      type = "gem";
    };
    version = "0.18.1";
  };
  mime-types = {
    dependencies = ["mime-types-data"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0ipw892jbksbxxcrlx9g5ljq60qx47pm24ywgfbyjskbcl78pkvb";
      type = "gem";
    };
    version = "3.4.1";
  };
  mime-types-data = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "003gd7mcay800k2q4pb2zn8lwwgci4bhi42v2jvlidm8ksx03i6q";
      type = "gem";
    };
    version = "3.2022.0105";
  };
  multi_xml = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0lmd4f401mvravi1i1yq7b2qjjli0yq7dfc4p1nj5nwajp7r6hyj";
      type = "gem";
    };
    version = "0.6.0";
  };
  mustache = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1l0p4wx15mi3wnamfv92ipkia4nsx8qi132c6g51jfdma3fiz2ch";
      type = "gem";
    };
    version = "1.1.1";
  };
  rake = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1cvaqarr1m84mhc006g3l1vw7sa5qpkcw0138lsxlf769zdllsgp";
      type = "gem";
    };
    version = "12.3.3";
  };
  rspec = {
    dependencies = ["rspec-core" "rspec-expectations" "rspec-mocks"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "19dyb6rcvgi9j2mksd29wfdhfdyzqk7yjhy1ai77559hbhpg61w9";
      type = "gem";
    };
    version = "3.11.0";
  };
  rspec-core = {
    dependencies = ["rspec-support"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "118hkfw9b11hvvalr7qlylwal5h8dihagm9xg7k4gskg7587hca6";
      type = "gem";
    };
    version = "3.11.0";
  };
  rspec-expectations = {
    dependencies = ["diff-lcs" "rspec-support"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0l1bzk6a68i1b2qix83vs40r0pbjawv67hixiq2qxsja19bbq3bc";
      type = "gem";
    };
    version = "3.11.1";
  };
  rspec-mocks = {
    dependencies = ["diff-lcs" "rspec-support"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "07vagjxdm5a6s103y8zkcnja6avpl8r196hrpiffmg7sk83dqdsm";
      type = "gem";
    };
    version = "3.11.1";
  };
  rspec-support = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1c01iicvrjk6vv744jgh0y4kk9d0kg2rd2ihdyzvg5p06xm2fpzq";
      type = "gem";
    };
    version = "3.11.1";
  };
}
