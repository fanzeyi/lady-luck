use std::collections::HashMap;

use maplit::hashmap;
use once_cell::sync::OnceCell;

pub fn skill_map() -> &'static HashMap<&'static str, &'static str> {
    static SKILL_MAP: OnceCell<HashMap<&'static str, &'static str>> = OnceCell::new();

    SKILL_MAP.get_or_init(|| hashmap! {
    "乔装" => "**乔装:** 使用在当你想要演出除你自己外的别人时。使用者改变了态度，习惯，以及/或声音来进行一个乔装，以另一个人或者另一类人的形象出现。戏剧化妆品可能会有所帮助，还有伪造的身份证。",
    "会计" => "**会计:** 使你理解会计工作的流程以及一个企业或者个人的金融职务。通过检查账簿，你可以发现做假账的员工，对资金的偷偷挪用，对行贿或者敲诈的款项支付，以及经济状况是否比表面陈述的更好或者更差。通过仔细检查旧账户，你可以了解过去的资金的得与失（谷物，奴隶贸易，威士忌酒的运营等）以及这些资金是付给了谁以及为了什么款项而支付。",
    "估价" => "**估价:** 用来估计某种物品的价值，包括质量，使用的材料以及工艺。相关的，技能使用者可以准确地辨认出物品的年龄，评估它的历史关联性以及发现赝品。",
    "侦查" => "**侦查:** 这项技能允许使用者发现密门或者秘密隔间，注意到隐藏的闯入者，发现并不明显的线索，发现重新涂过漆的汽车，意识到埋伏，注意到鼓出的口袋，或者任何类似的事情。对于调查员来说，这是一个很重要的技能。

如果一名角色仅有很短的时间来进行侦查，例如飞奔经过对方时，KP可能会提升难度等级。如果一名角色正在进行一场完整的调查，那么KP也许会允许一个自动成功。这项技能的难度等级同样也会环境的情况来调整，在一个杂乱的房间中进行侦查将会更加困难。",
    "医学" => "**医学:** 使用者可以诊断并治疗事故，创伤，疾病，毒药等，并且可以提供公共健康建议。如果一个时代还并没有好的治疗某种疾病的疗法，那么这项技能的效果是有限的，不确定的，或者无效的。医学技能能给予大范围的对于药片以及药剂，是自然还是人造的知识，以及对副作用以及禁忌症状的理解。

用医学技能来进行治疗最少要花费1小时时间，并且可以在造成了伤害后的任何时间进行处理，但是如果这并没有在同一天内进行处理，难度等级将会上升（需要一个困难难度的成功）。一名角色如果被成功地用医学技能进行治疗，他将恢复1D3的生命值（除在任何接受的急救之外的），除了在一名濒死角色的场合，他必须先接受一个成功的急救技能检定来稳定伤势，然后才能接受一个医学检定。

一名角色被限定只能接受一次成功的急救以及医学的治疗，直到遭受了进一步的伤害（除了在濒死角色的场合，他可能需要多次的急救检定来稳定伤势）。成功的医学技能的使用可以将一名昏迷的角色从昏迷中唤醒。

当处理重伤时，成功的医学技能的使用可以提供病人在每周的恢复检定上一个奖励骰。

KP可能准许医学治疗自动成功，如果是在一个当代的，设备完善的医院中。",
    "历史" => "**历史:** 让一名调查员能够记住一个国家，城市，区域或者个人及其相关的重要情报。一个成功的检定可以用来帮助辨认先祖所熟悉的工具，科技，或者想法，但是对当下的所知甚少。",
    "取悦" => "**取悦:** 取悦允许通过许多形式来使用，包括肉体魅力、 诱惑、奉承或是单纯令人感到温暖的人格魅力。取悦 可能可以被用于迫使某人进行特定的行动，但是不会 是与个人日常举止完全相反的行为。取悦或是心理学 技能可以用于对抗取悦技能。

取悦技能可以被用于讨价还价来使一件物品或者 服务的价格降低。如果成功，使用者得到了卖家的赞 同，并且他们可能乐意降低一点价格。",
    "妙手" => "**妙手:** 允许对物体进行视觉上的遮住，藏匿，或者掩盖，也许通过残害，衣服或者其他的干涉或促成错觉的材料，也许通过使用一个秘密的嵌板或者隔间。任何种类的巨大物件应当增加藏匿的难度。

妙手包括偷窃，卡牌魔术，以及秘密使用手机。",
    "导航" => "**导航:** 允许使用者在早上或者晚上，在暴风雨或者晴朗天气中认清自己的路。有着更高技能的人将对天文表图和工具，以及卫星定位装置十分熟悉，如果他们是在有着那些东西的时代的话。一名角色也可以用这项技能来测量以及对一块区域进行绘图（制图学），判断是有着几平方米的小岛或者是一块内陆区域—使用现代工具可以降低甚至取消难度等级。

KP可以将这个技能的检定作为隐藏骰进行处理调查员需要尝试去解决的一件事情，并且最后承受结果。

如果角色对该区域十分熟悉，那么在这个检定上可以得到一个奖励骰。",
    "急救" => "**急救:** 使用者有能力可以提供紧急的医疗处理。这可能包括：对摔断了的腿用夹板进行处理，止血，处理烧伤，对一名溺水的受害者进行复苏处理，包扎以及清理伤口等等。急救不能用于治疗疾病（这需要医学技能）。

为了能做到有效，急救必须在一小时内进行处理，在这情况下，能回复1生命值的损伤。这项技能可以尝试一次，并且后续的尝试将为进行孤注一掷。 两个人可以合作进行急救，只要其中一人成功便可以得到生命值的回复。成功的急救的使用可以将一名昏迷的角色唤醒过来。一名角色被限制只能进行一次成功的急救和医学，直到受到其他伤害。

当处理一名濒死的角色，成功的急救可以稳定他的状态一小时，并且得到一点临时生命。在一小时结束后，在那之后每经过一小时，那名角色必须进行一次成功的体质（CON）检定来维持伤势的稳定，否则 （如果体质检定失败）那名角色陷入濒死并且失去临时生命，之后每轮必须进行一次体质（CON）检定来避免死亡。如果那名角色存活到下一轮，可以再次尝试对其使用急救（最多可以两人使用）。这个可以不断持续下去（不算是使用孤注一掷）直到伤势被稳定或者其死亡。

急救（并且只有急救）可以拯救一名濒死角色的生命，在之后他必须接受一个成功的医学检定或者被送往医院。因此，急救是一个需要拥有的重要的技能，伴随着医学，如果你希望能成为一名有用的医生。",
    "恐吓" => "**恐吓:** 恐吓可以以许多形式使用，包括武力威慑，心理 操控，以及威胁。这通常被用来使某人害怕，并迫使 其进行某种特定的行为。恐吓的对抗技能为恐吓或者心理学。

携带武器或者其他的有力的威胁或诱因来协助恐吓可能可以降低难度等级。当在恐吓上使用孤注一掷时，失败的可能结果之一是对目标进行了远远超过本身意图的恐吓。

恐吓可以被用于降低一件物品或者服务的价格。 如果成功，卖家可能会降低价格，或者免费交出，但是根据情况，对方可能会将这事情举报给警察或者当地犯罪组织的成员。",
    "投掷" => "**投掷:** 当需要用物体击中目标或者用物件的正确部分击中目标（例如小刀或者短柄小斧的刃）时，使用投掷技能。手掌大小、质地均匀的物体（或任何为投掷设计的武器）可以被掷出力量除以5码远的距离。有效射程可以增加至力量除以2码，但投掷检定会因此承受一颗惩罚骰。如果要投掷的物体较重（超过力量除以10磅），则以上射程的单位从码变为英尺。

如果投掷技能检定失败，投掷物将会掉落在距离目标随机距离的地方。KP应当将骰子检定数与最高的能够达成成功的数值相比较，然后判断投掷物落在目标和投掷者之间合适的距离的地方。

投掷技能被用于在战斗中投掷小刀，石头，投矛或者回力标时。",
    "攀爬" => "**攀爬:** 这项技能允许一名角色借助或者不借助绳索或者登山工具进行爬树、墙以及其他垂直表面。这项技能也同样包括用绳索下降。

攀爬表面是否坚固，是否有可以用手握住的地方，风力，可见度，雨等等坏境状况都可能会影响难度等级。

第一次在这个技能上失败可能意味着这攀爬超出了调查员的能力范围。在孤注一掷上失败则可能意味着摔落了下来，同时因此受到伤害。一个成功的攀爬检定应当允许调查员在任何场合下完成攀爬（而不是进行反复检定）。一次富有挑战性或者长距离的攀爬则应当增加难度等级。",
    "母语" => "**母语:** 当选择这项技能时，必须明确一门具体的语言并 且写在技能的后面。在婴儿期或者童年早期，大多数人使用单一一门语言。玩家所选择作为母语的语言自 动地以等同于调查员教育（EDU）属性为起始；此后，调查员以那个百分比或者更高的来进行理解， 说，读以及写（如果更多的技能点数在调查员创作时加了上去）。",
    "法律" => "**法律:** 代表你对相关法律、早期事件、法庭辩术或者法院程序了解的可能性。一个在法律实务上的专家可能会获得巨大的奖励以及政治事务所，但是这可能需要长达几年的认真申请—一个较高的信用评级在这关系上也十分重要。在美国，一个州的州法庭（State Bar）必须批准某人的法律实务。

当到一个外国国家时，使用这项技能的难度等级可能会上升，除非这名角色花费数月的时间来学习这个国家的法律系统。",
    "游泳" => "**游泳:** 有能力在水或者其他液体中漂浮以及移动。只有在遭遇危险的时候需要进行游泳技能检定，或者当KP认为合适的时候。当进行游泳的孤注一掷失败时，可能会导致生命值的损失。也可能会导致人物被顺着水流向下冲走，被水流半淹或者完全淹没。",
    "潜水" => "**潜水:** 使用者接受过在深海游泳的使用以及维持潜水设备的训练，水下导航，合适的下潜配重，以及应对紧急情况的方法。

在1942年的水肺（潜水氧气筒）发明前，严格的潜水套装是装备着能从水面输送空气的连接管道。

在现代，一名水肺潜水员将会熟悉当呼吸增压氧气时发生的潜水时的物理现象，气压，以及生理学的过程。",
    "潜行" => "**潜行:** 安静地移动以及/或者躲藏的技巧，不惊扰到那些可能在听或者看的人们。当尝试躲避探查，玩家应当进行一个潜行的技能检定。与这项技能相关的能力意味着要么角色能够安静地移动（轻声轻足）以及/或者在伪装技巧上有所训练。这项技能也同样意味着角色可以在长时间维持一定程度的谨慎心态以及冷静的头脑来使自己保持静止和隐秘。",
    "炮术" => "**炮术:** 这项技能呈现出对一些形式的军事训练和经历。 使用者具有在战争中操作战地武器的经验：可以在一 个工作队或“派遣队”中工作，进行对超过个人武器射 击距离的武器的操作。这些武器通常过于巨大以至于 无法单人进行操作，并且个人无法再没有工作队支援 的情况下使用这武器，或者应当提高难度等级（取决 于KP的判断，也取决于使用的武器类型）。

存在着许多不同的武器专攻，取决于游戏设定的 时期，包括加农炮、榴弹炮、迫击炮以及火箭发射 器。",
    "爆破" => "**爆破:** ",
    "聆听" => "**聆听:** 衡量一名调查员理解声音的能力，包括偶然听到 的对话，一扇关着的门后的轻声嘀咕，以及咖啡厅里 的私语。KP可以用这来决定一场即将发生的遭遇的形 式：是你的调查员因被踩碎的树枝的声音而警觉到了 到来的遭遇？甚至此外，一个较高的聆听技能可以指 一名角色有着较高的警觉能力。",
    "话术" => "**话术:** 话术特别限定于言语上的哄骗，欺骗以及误导， 例如迷惑一名门卫来让你进入一间俱乐部，让某人在一张他还没有读的文件上签字，误导警察看向另一边，以及诸如此类的。这项技能的对立技能为心理学 或者话术。经过一段时间的相信期后（通常在使用话术的人离开场景之后），对方会意识到自己被欺骗了。话术的效果总是暂时性的，尽管如果达成了更高的难度等级可能会使这个效果更加长一点。

可以被用来对一件物品或者服务的价格进行砍价。如果成功，卖家会暂时性地觉得这是一场不错得交易；然而，如果买家打算归还或者试图购买别的物品，卖家可能会拒绝继续提供降价，并且甚至可能会提高价格为了补回他们在上一次交易中所造成的损失！",
    "说服" => "**说服:** 使用说服来通过一场有理有据的论述、争辩以及讨论让目标相信一个确切的想法，概念，或者信仰。说服并不一定需要涉及真实的内容。成功的说服技能的运用将花费不少的时间：至少半小时。如果你想快速地说服某人，你应该使用话术技能。

取决于玩家表述的目标，如果调查员花费了足够的时间，说服造成的影响可能一直持续下去，并且无意识地影响着别人；可能会持续好几年，直到某件事件或者另一次得说服改变了目标的想法。

说服可以被用于讨价还价，以此来削低某样物品或者服务的价格。如果成功，卖家将会完全地相信自己做了一场好买卖。",
    "读唇" => "**读唇:** 这项技能允许好奇的调查员听懂一段交谈对话， 而不需要听见对方说了什么。能看到对方的视线是必须的，并且如果只能看到其中一名说话者的唇（另一名可能只能看到背），那么只能辨认出一半的对话。读唇也可以用于与另一个人进行无声的交流（如果双方都是专家），允许相对更加复杂的短语以及含义。",
    "跳跃" => "**跳跃:** 如果成功，调查员可以在垂直方向上跳起或跳 下，或者从一个站立点或起步点水平向外跳。当坠落 时，跳跃可以被用来降低可能造成的坠落伤害。

为了分辨哪些算在正常跳跃，困难跳跃以及极难 跳跃（分别需要一个常规难度成功，困难难度成功，以及极难难度成功），必须对判断进行训练。作为一 个指导，当调查员想要安全地从垂直等同于其自身高 度的地方跳下来时，需要一个常规难度的成功，或者 水平地从其站立点跳过长度等同于他自身高度的坑， 或者助跑后跳过两倍于其自身高度的距离。如果要达 成两倍距离的跳跃，则需要一个极难难度的成功，尽 管应当牢记，最长跳跃的世界纪录为大约29英尺。

如果从高处摔落下来，一个成功的跳跃检定可以 使对坠落有所准备，降低一半的坠落伤害。",
    "追踪" => "**追踪:** 一名调查员可以凭借追踪技能来通过土壤上的脚印，或是物体通过植被时留下的印记来追踪别人，或者是交通工具以及地球上的动物。时间的经过，雨，以及土地的种类都可能会影响追踪的难度等级。",
    "锁匠" => "**锁匠:** 锁匠技能可以打开车门，短路电线来发动汽车，用铁撬撬开图书馆的窗子，解决中国机关箱，以及穿过常规的商用警报系统。使用者可能会修复锁，制作钥匙，或者在万能钥匙，开锁工具或者其他工具的帮助下打开锁。特别困难的锁可能会需要一个更高的难度等级。",
    "闪避" => "**闪避:** 允许调查员本能地闪避攻击，投掷过来的投射物以及诸如此类的。一名角色可以尝试在一场战斗轮中使用任何次数的闪避（但是对抗一次特定的攻击只能一次；见[[战斗]]）。闪避可以通过经验来提升， 就像其他的技能一样。如果一次攻击可以被看见，一 名角色可以尝试闪避开它。想要闪避子弹是不可能的，因为运动中的它们是不可能被看见的；一名角色所能做到的最好的是做逃避的行动来造成自己更难被命中（“寻找掩体（Diving for cover）“）。",
    "骑术" => "**骑术:** 这项技能被用于给坐在鞍上驾驭马，驴子或者骡子，以及获得对这些骑乘动物、骑乘工具的基础照料知识，以及如何在疾驰中或困难地形上操纵坐骑。当坐骑出乎意外地抬起身子或失足时，骑手保持自己在坐骑上不摔落的几率等同于他的骑术技能。偏坐在马鞍上进行骑乘将会提高一个等级的难度等级。对于不熟悉的坐骑（例如骆驼）也可以成功地骑乘，但是可能会需要更高的难度等级。

如果一名调查员从坐骑上摔落下来，可能是因为坐骑垮了，摔落了或者是死了（或者因为骑术的孤注一掷检定失败），这次意外将造成至少1D6生命值的损失—尽管跳跃检定可以抵消这个损失。",
    "人类学" => "**人类学:** 使使用者能够通过观察来辨认和理解一个人的生 活方式。如果技能使用者持续观察一个其他的文化一 段时间，或者在有着关于某种已消失文化的正确资料 环境下工作，那么他可以对文化方式以及道德习惯进 行简单的预测，即使证据可能并不完整。通过学习文 化一个月或者更久，人类学家开始理解这种文化是如 何运作的以及，如果结合心理学，可以预测那些研究 文化的行为和信仰。",
    "博物学" => "**博物学:** 起初指对于在自然环境中的植物以及动物生命的研究。直到19世纪，这门学科被分开到一系列的学术学科（生物学，植物学，等等）。作为一个技能，博物学达标了传统的（非科学的）知识以及农民，渔民，优秀的业余者，以及单纯的爱好者的个人观察。 它可以一般地对物种，栖息地进行辨认，并且可以辨认踪迹、足迹以及叫声，也可以允许对什么事物可能对某种特定物种来说很重要进行猜测。如果要一个对自然世界的科学性的理解，那么应当去看生物学，植物学以及动物学技能。

博物学可能准确也可能不准确—这只是评估，判断，民间传统，以及热衷的事物的领域。使用博物学来判断集市中的马肉，或者决定是否蝴蝶收集品非常棒或者只是非常棒地排列了起来。",
    "心理学" => "**心理学:** 对所有人来说都很通用的察觉方面的技能，允许使用者研究个人并且形成对于其他某人动机和人格的了解。KP可以选择替代玩家暗骰心理学技能，根据检定结果，向玩家声明真或假的信息（不告知玩家检定成功与否以及信息的真伪）。",
    "电子学" => "**电子学:** 用来发现并对电子设备的故障进行维修。允许制作简单的电子设备。这是个现代技能—在1920年代则是使用物理学以及电气维修来应对电子设备。

不像电气维修技能，电子学工作的部件通常是不 能临时配备的：它们通过精密的工作被设计出来。通 常如果没有正确的微晶片或者电路板，技能的使用者就无法进行工作，除非他们可以策划出一些形式的应急方案。",
    "神秘学" => "**神秘学:** 使用者可以识别出神秘学道具，用语和概念，以及民间传统，并且可以辨认魔法书以及神秘学记号。神秘学家对于代代相传的各类神秘知识十分熟悉，包括从埃及和苏美尔，从中世纪和文艺复兴时期的西方，以及也许从亚洲或者非洲。

理解特定的书籍可能可以增加神秘学技能的百分比。这项技能不能运用于与克苏鲁神话相关的咒术，书本，以及魔法，尽管旧日支配者的崇拜者对于神秘学有着很高的接受能力。

由KP决定在这场游戏中非神话魔法是真实存在的或者是虚构的（见[[魔法]]）。",
    "考古学" => "**考古学:** 允许从过去的文化中鉴定一件古董的年代以及辨别它，以及可以用来发现赝品。使获得建立以及开掘一个挖掘遗址的专业知识。通过对遗址的勘察，使用者可以推断留下这遗址的生物的目的和生活方式。人类学可能对此会有所帮助。考古学还有助于辨认已消失的人类语言的书面形式。",
    "信用评级" => "**信用评级:** 衡量了调查员表现出来的富裕程度以及经济上的自信度。钱是敲门砖；如果调查员尝试用他的经济地位来达成某个目标，那么也许使用信用评级技能会比较合适。信用评级可以被用来取代APP来评估第一印象。

与其说是技能，信用评级到更像是评估富裕程度的而指标，它不会像其他技能那样获得成长标记。一个高信用评级在游戏中将会是一个有用的资源，并且应当在创造调查员时加上一定的点数。每个职业有着起始的信用评级范围，并且应当花费技能点来达到这个评级范围内。

克苏鲁的呼唤并不是一个需要仔细追溯金钱的游戏；然而，了解一名调查员的经济能到什么程度也是很有用的—例如，一个调查员是否足以支付得起雇佣一队考古学家以及工作人员来挖掘一个埃及坟墓？一名调查员的信用评级可以随着时间而改变。调查员的克苏鲁神话技能有着易于疯狂的倾向，而这个技能可能导致失业并因此变成一个更低的信用评级。见[[信用评级与调查员的支出]]来获得一个更加深入的对信用评级及其使用的了解。",
    "动物驯养" => "**动物驯养:** 命令以及训练已驯化动物去完成一些简单任务的技能。这个技能最常用于狗上，但也包括鸟、猫、猴子以及其他（取决于KP的判断）。至于对动物的骑乘，例如马或者骆驼，则要用骑术技能来进行行动以及操控这些坐骑。",
    "机械维修" => "**机械维修:** 这项技能允许调查员修理一个破损的机器或者制造一个新的。基础的木工手艺和管道项目也可以执行，制作物品也同样可以（例如一组滑轮系统）以及维修物品（例如蒸汽泵）。在使用技能中可能会需要特殊的工具或者部件。这项技能可以用来打开普通的家庭锁，但是更加专业的就不能了—见锁匠技能来打 开更加复杂的锁。机械维修是一个与电气维修相伴随的技能，并且两者都可能需要来为了修理一个复杂的设备，例如汽车或者飞行器。",
    "汽车驾驶" => "**汽车驾驶:** 任何有着这项技能的人都可以驾驶一辆汽车或者轻型卡车，进行常规的移动，并且处理机动车的一般毛病。如果调查员想要甩掉一名追踪者或者追踪某人，则需要一个汽车驾驶检定。

一些其他的文化可能用相似的事物来取代这个技能；因纽特人可能使用狗撬驾驶，或者维多利亚人可能使用四轮马车驾驶。",
    "电气维修" => "**电气维修:** 使调查员能够修理或者改装电气设备，例如自动点火装置，电动机，保险丝盒，以及防盗自动警铃。 在现代，这项技能对现代电子器件几乎做不到什么。 为了维修电气设备，可能需要特殊的部件或者工具。 在1920年代的职业可能会需要这个技能，并且需要机械维修技能作为组合。

电气维修也可能在现代的爆破上被使用，例如雷管，C-4塑料炸弹，以及地雷。这些武器被设计得简单易用；只有一个大失败的结果才会造成不启动（记住这检定可以使用孤注一掷）。拆除爆炸物是远远更为复杂的，因为它们可能被安装了反拆改装置；当用 于解除爆炸物时应当提高难度等级—见[[爆破]]技能。",
    "精神分析" => "**精神分析:** 这项技能指的是广泛的情感上的治疗，不单是弗洛伊德的疗法。在1890年代，正规的心理治疗仍处于发展的初期阶段，尽管一些疗法有着人类存在般悠久的历史。一些时候，这看上去像是一门欺诈性的研究，即使是在1920年代。在之后用来称呼那些精神病医师或者对情绪障碍进行研究的学者的通用术语为精神病学家（alienist）。在现代，心理治疗的各种方面都有了很大的发展，并且这项技能已经不能仅仅用精神病治疗来命名了。

短期强化的精神分析可以恢复一名调查员患者的理智值。进行心理治疗时，游戏时间每月一次，精神病医师或医生进行一次精神分析技能检定。如果成功了，病人恢复1D3的理智值。如果检定失败了，没有任何恢复。如果检定为大失败，那么病人失去1d6的理智值，并且由心理医师进行的治疗到此结束：可能在心理治疗中发生了一些严重的事变或者戏剧性的阻碍，并且在病人与治疗专家之间的关系破损到了难以修复的地步。

在游戏中，单独的精神分析并不能加速不定性疯狂的恢复，恢复需要1D6个月的系统全面（或者相似的）的照顾，而精神疗法只是构成了其中的一部分。

成功使用这项技能将允许角色在短期内克服恐惧症状，或者看穿幻觉。在游戏中，这允许一名疯狂的调查员在短期内免受恐惧症或者躁狂症的影响，例如允许一名幽闭恐惧症患者躲藏在扫把柜中十分钟。同样的，一名角色可以进行一个精神分析检定来帮助一名处于妄想中的调查员在短期内看破幻觉不受影响。

由一名心理治疗专家进行的治疗可以在不定性疯狂期间内回复理智值（见[[理智]]）。

精神分析无法将一名角色的理智值加到超过99克苏鲁神话。",
    "克苏鲁神话" => "**克苏鲁神话:** 这项技能反应了对非人类（洛夫克拉夫特的）克苏鲁神话的了解。这个技能并不像学术技能一样建立 在知识的积累之上。相反地，它代表了克苏鲁神话向 人类思想的打开以及同化。因此，克苏鲁神话技能因 遇见深潜者（举例来说）而取得可以转换成其他情况 以及生物。同样也指像是“哪些是人类不该知道的”， 克苏鲁神话是与人类的理解相对立的，并且接触它将 会侵蚀人类的理智。

没有调查员能在初始技能设定时给克苏鲁神话加 点（除非被KP同意）。因此在调查员表单上没有给克苏鲁神话的勾选格，因为成功的使用克苏鲁神话技能 并不会提供给调查员在这个技能百分比上的提升。取 而代之的，在克苏鲁神话上的点数将通过与神话生物 相遇而导致的疯狂获得，通过洞察了这个宇宙真实世 界而造成的疯狂，以及通过阅读了禁书以及其他的神 话书作。一名角色的理智永远不可能超过99减去他的 克苏鲁神话技能。当克苏鲁神话点数上升，它将减少 理智上限，并且使得调查员变得脆弱。

每当神话生物的足迹或者其他证据被发现，一个 成功的克苏鲁神话检定可以允许调查员辨认出这个神 话生物，推测出有关它行为的一些资讯，或者猜测出 它所拥有的某些特性。一个成功的克苏鲁神话检定也 可能允许调查员回想起一些关于神话的真实，通过看 见咒语的施展来辨认出它，回想起克苏鲁书卷中详细 的咒语或者部分的信息，或者完成一些其他的任务。 克苏鲁神话技能也可以被用来展现出魔法的“咒语一样 的”效果。",
    "图书馆使用" => "**图书馆使用:** 图书馆使用使一名调查员能在图书馆找到一些信息，例如特定的一本书，新闻或者参考书，搜集文件或者资料库，假设需要的东西确实在那里的话。使用这个技能需要数小时的连续的调查。

这项技能可以定位寻找一件隐藏的案例或者一本特殊收藏的稀有书籍，但是说服，话术，取悦，恐吓，信用评级，或者特殊的证明书可能会需要来获得阅读这书或者信息的许可。",
    "艺术与手艺" => "**艺术与手艺:** 调查员表单上有着专门的空白空间用来写下这门技能的技能专攻。例如：

* 写作
* 理发
* 莫里斯舞蹈
* 书法
* 歌剧歌唱
* 木匠
* 粉刷匠和油漆工
* 厨艺
* 摄影
* 舞蹈
* 制陶
* 美术
* 雕塑
* 伪造
* 吹真空管

许多这些例子都是与专业直接相联系的技能，但是这些技能可能只是休闲嗜好。你可以花费技能在任意技能专攻上。不可以加点在作为类属的“艺术与手艺”上。

这项技能可能能使你制作或者修理一样东西—通常需要工具和时间，由KP来决定，如果必要的话。在适用成功程度分度的情况下，一个更高等级的成功表示这件物品有着高品质以及/或精致度高。

一个艺术或者手艺技能可能可以用于制作一个复制品或者赝品。在这情况下，难度等级将取决于需要复制的原品的复杂程度以及独特性。在伪造文件的场合下，将使用一个专门的技能专攻（伪造）。

一个成功的检定可能可以提供一个物品的相关信息，例如这个物品在何时以及哪里被制造，与之相关的一些历史或者技艺，或者谁可能制作了它。拥有这个技能的专家将会在某个专门的领域里有着广泛的知识—对于物品本身，它的历史以及当代从事相关行业的人的知识，以及去实践知识的能力。",
    "计算机使用" => "**计算机使用:** 这项技能允许调查员用各种不同的电脑语言进行编程；恢复或者分析隐藏的数据；解除被加了保护的系统；探索一个复杂的网络；或者发现别人的骇入、 后门程序、病毒。对电脑系统的特殊操作可能会需要这个检定。

互联网将大量的信息放置在了调查员的指尖上。 使用互联网来找到高度详细以及/或模糊不清的咨询可能会需要一个计算机使用和图书馆使用的组合检定。

这项技能在用电脑上网，检查电子邮件，或者运行一般的商品化软件时不需要使用。",
    "学识" => "**学识（专攻）:** 这项技能代表一名觉得对一个超出人类常规知识范畴的事物的专业理解。学识的技能专攻应该具体并且不同寻常，例如：

- 梦学识
- 死灵之书学识（e.g. 历史的）
- UFO学识
- 吸血鬼学识
- 狼人学识
- 亚狄斯星人学识

当KP想要测试某位调查员归属于于这些领域学识其中之一的某物的知识时，但是调查员缺少相关的专业学识，KP可以允许一个其他的（更加通用的）技能的使用，但是需要一个更高难度等级的成功。例如， 如果KP测试一名现代调查员对于1980年代外星人诱拐的知识，他可以要求一个常规难度的UFO学识的成功检定，或者一个困难难度的历史技能的成功检定。

对于KP来说，学识技能也可以用作对表示一个非玩家角色知识量的简单记录。在主流中，知识由教育属性（EDU）以及专门的技能来表示，例如历史或者克苏鲁神话。KP应当决定何时应当将学识技能放入游戏中—通常只有当一块特殊领域的专业知识成为整场游戏核心的时候。",
    "射击" => "**射击（专攻）:** 包括了各种形式的火器，也包括了弓箭和弩。你 可以花费技能点数来获得任何技能专攻。作为属类的 “射击”技能不能被获得。选择与你调查员的职业与时 代历史相契合的技能专攻。

注释：作为一个战斗技能，这项技能不存在孤注一掷。",
    "操作重型机械" => "**操作重型机械:** 当驾驶以及操纵一辆坦克，反铲挖土机，蒸汽挖土机或者其他巨型建造机械时需要这个技能。对于种类非常不同的机械，KP可以决定提高难度等级，如果遇到的问题是极大程度上不熟悉的；例如，过去常常开推土机的某人，不会立刻能够掌握对船的引擎舱中的蒸汽涡轮机的使用。",
    "格斗" => "**格斗（专攻）:** 格斗技能指的是一名角色在近距离战斗上的技能。你可以花费一定的点数来获得任何技能专攻。作为类属的“格斗”技能不能够获得。选择对你的调查员的职业以及当时历史合适的专业格斗技能。

那些有着50%或者以上的格斗（斗殴）技能的调查员可能会希望选择一种正规的训练，并且作为背景的一部分来对他们的技能程度进行解释。格斗方式存在着各种各样的。武术只是单纯的一种提升一个人战斗技巧的方式。决定角色是如何学习格斗的，是否是从正规的军队训练，武术教室，或者以自己的努力从街头格斗中学会。术语斗殴可能会觉得对于作为一 个技能化的武术来说过于粗糙，并且可以被代替（例如用“空手道”），如果玩家是这样希望的话。

注释：作为一个战斗技能，这个技能不存在孤注一掷。",
    "生存" => "**生存（专攻）:** 提供专业的如何在极端环境下生存的知识和技巧，例如在沙漠中或者极地环境，也包括海洋上或者荒野。内容包括狩猎的知识，搭建住所，可能遇到的危险的知识（例如如何避开有毒性的植物）等等，取决于所处的环境。

你可以花费技能点来获得任何的技能专攻。作为属类的“生存”技能本身不能被获得。专业环境的生存技巧应当在技能选择时就决定下来，例如：生存 （沙漠），（海洋），（极地），等等。当一名角色没有明显对应的生存专业技能，他可以使用相似的专业进行检定，但是将会提升难度等级（或者惩罚骰），取决于KP的判断。",
    "科学" => "**科学（专攻）:** 科学专业上的理论和实践的能力，拥有这个技能的人接受过一定程度的正式的教育或者训练，尽管一名博览群书的业余科学家也是可能存在的。对于知识的理解和认识受到游戏时代的限制。你可以花费点数来获得任何你想要的技能专攻。作为属类的“科学”技能不能被获得。

每个技能专攻包括了一门专门的学科，并且列表所给出的并不是全部。许多专业跨越了不同的知识领域，并且有所重叠，例如数学和密码学，植物学和生物学，化学和药学。当一名角色没有完全对应的专业学科技能，他可以用一个相似的技能进行检定，但是由KP来判断是否要增加难度等级（或者一个惩罚骰）。",
    "驾驶" => "**驾驶（专攻）:** 相当于水上或者空中的汽车驾驶，这时使用空中飞行或者水上航行交通供给的技巧。你可以花费技能点来获得任何技能专攻。作为属类的驾驶技能不能被获得。一名调查员可以在调查员角色卡的空档下写上许多不同种的这一技巧（例如驾驶飞行器，驾驶飞艇等等）。每个的初始都是01%。

任何有着适当程度技能的人可以在一个平静且有着良好可见度的日子里进行航行或者飞行，然而在暴风雨的天气下，需要用到导航仪器时，低可见度下或者其他困难的处境时会需要用到技能检定。不良的天气，极差的可见度，以及器材的损伤都可能会提高驾驶飞行器或水上航具的技能检定的难度等级。",
    "其他语言" => "**其他语言（专攻）:** 当选择这个技能，必须明确一个具体的语言并且写在技能后面。一个人可以了解任何数量的语言。这项技能代表使用者可以了解，说，读以及写一门不是他母语的语言的可能性。

远古或者不知名语言（例如Aklo，Hyperborean 这两种神话语言）不能被选择（除非KP同意），但是通常意义上的早期语言可以被选择。KP可以提高难度等级，如果遇见了用那门语言的古式的演讲或者写作。单次的其他语言技能检定的成功通常允许对整本书的理解。 对其他语言技能的建议： ·在一门语言上5%的技能将能够正确地辨认出这 门语言而不需要检定。

* 在一门语言上10%的技能可以交流简单的想法。
* 在一门语言上30%的技能可以对社交上的需求进行理解。
* 在一门语言上50%的技能可以进行流畅的交流。
* 在一门语言上75%的技能可以将一门外国语言说得像是本地人一样。
* 辨认一门当下在使用的人类语言（调查员所不清楚的），使用知识检定。
* 辨认一门已消逝的人类语言，使用考古学或者历史检定。
* 辨认一门外星语言，使用克苏鲁神话，或者也许神秘学检定。",
    })
}