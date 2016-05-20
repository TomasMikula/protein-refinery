package protein.capability

import protein.mechanism.{ModifiedProtein, Site}

case class Agents(agents: List[ModifiedProtein], bonds: List[(Int, Site, Int, Site)])
